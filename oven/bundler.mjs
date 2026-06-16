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
import { fileURLToPath } from "url";

// ─── Configuration ──────────────────────────────────────────────────

// Source directory for aesthetic.computer platform files.
// In production this is rsynced by deploy.sh to /opt/oven/ac-source/
// In dev, override with AC_SOURCE_DIR env var.
const AC_SOURCE_DIR = process.env.AC_SOURCE_DIR
  || path.resolve(process.cwd(), "ac-source");
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const BOX_ART_FONT_STACK = "'KidLispComic', 'Comic Relief', 'Comic Sans MS', 'Comic Sans', cursive";
const BOX_ART_COMIC_FONT_FILE = path.join(__dirname, "fonts", "ComicRelief-Bold.ttf");
let comicReliefBoldBase64 = "";
try {
  comicReliefBoldBase64 = fsSync.readFileSync(BOX_ART_COMIC_FONT_FILE).toString("base64");
} catch {
  // Falls back to Comic Sans stack when oven font assets are unavailable.
}

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
let coreBundleCacheBuiltAt = 0;
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
  "disks/prompt.mjs", // 377 KB — stubbed below (not needed in PACK mode)
  "disks/chat.mjs",   // 184 KB — stubbed below (chat UI, not functional in PACK_MODE)
];

// Lightweight stubs injected into VFS to satisfy static import chains
// without bundling the full 560+ KB of source.
// lib/disk.mjs dynamically imports "../disks/chat.mjs" (dead code: chatEnabled = false)
const VFS_STUBS = {
  "disks/prompt.mjs": ``,
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

function normalizeHandle(handle) {
  if (typeof handle !== "string") return null;
  const cleaned = handle.trim().replace(/^@+/, "");
  return cleaned || null;
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
  return {
    source: data.source,
    userId: data.user || null,
    authorHandle: normalizeHandle(data.handle),
  };
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
  let mainPieceAuthorHandle = null;

  while (toProcess.length > 0) {
    const current = toProcess.shift();
    const cleanName = current.replace("$", "");
    if (processed.has(cleanName)) continue;
    processed.add(cleanName);

    const { source, userId, authorHandle } = await fetchKidLispFromAPI(cleanName);
    allSources[cleanName] = source;

    if (cleanName === pieceName.replace("$", "")) {
      if (userId) mainPieceUserId = userId;
      if (authorHandle) mainPieceAuthorHandle = authorHandle;
    }

    const refs = extractKidLispRefs(source);
    for (const ref of refs) {
      const refName = ref.replace("$", "");
      if (!processed.has(refName)) toProcess.push(refName);
    }
  }

  let authorHandle = mainPieceAuthorHandle || "anon";
  let userCode = null;
  if (mainPieceUserId) {
    const info = await fetchAuthorInfo(mainPieceUserId);
    if (!mainPieceAuthorHandle && info.handle) {
      authorHandle = normalizeHandle(info.handle) || authorHandle;
    }
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
  // Path char classes exclude `?` so greedy matching stops at the query
  // string boundary — otherwise `./foo.mjs?v=123` becomes the path and the
  // rewritten specifier (with query) doesn't match the import map's
  // query-less VFS keys, causing runtime resolve failures.
  code = code.replace(
    /from\s*['"](\.\.\/[^'"?]+|\.\/[^'"?]+)(\?[^'"]+)?['"]/g,
    (match, p) => {
      const resolved = resolvePath(filepath, p);
      return 'from"' + resolved + '"';
    }
  );
  code = code.replace(
    /import\s*\((['"](\.\.\/[^'"?]+|\.\/[^'"?]+)(\?[^'"]+)?['")])\)/g,
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
    /\(\s*['"](\.\.?\/[^'"?]+\.m?js)(\?[^'"]*)?['"]\s*\)/g,
    (match, p) => {
      const resolved = resolvePath(filepath, p);
      return '("' + resolved + '")';
    }
  );

  // Rewrite new URL("relative-path", import.meta.url) patterns.
  // In pack mode, import.meta.url is a blob: URL that can't resolve relative paths.
  // Resolve the relative path to a VFS-absolute path so the fetch intercept can serve it.
  code = code.replace(
    /new\s+URL\(\s*['"](\.\.?\/[^'"]+)['"]\s*,\s*import\.meta\.url\s*\)/g,
    (match, p) => {
      const resolved = resolvePath(filepath, p);
      return 'new URL("/' + resolved + '", location.href)';
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
        passes: 2,
        pure_getters: "strict",
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
      // Exclude `?` from the path char class so cache-bust queries
      // (`./foo.mjs?v=123`) don't get baked into the resolved VFS key —
      // otherwise `fs.readFile('lib/foo.mjs?v=123')` fails and the file is
      // silently dropped from the bundle.
      const importRegex = /from\s+["'](\.\.[^"'?]+|\.\/[^"'?]+)(?:\?[^"']*)?["']/g;
      const dynamicRegex = /import\s*\(\s*["'](\.\.[^"'?]+|\.\/[^"'?]+)(?:\?[^"']*)?["']\s*\)/g;

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

  // Auto-invalidate when ac-source has been rsynced since the last cache build
  // (sync-source.sh runs without restarting this Node process, so the cache
  // would otherwise serve stale-bundler output until prewarm is hit). Stat one
  // hot file as a low-cost proxy for "the source tree changed."
  let stale = false;
  if (coreBundleCache && coreBundleCacheBuiltAt > 0) {
    try {
      const sentinel = path.join(acDir, "lib/disk.mjs");
      const mtime = fsSync.statSync(sentinel).mtimeMs;
      if (mtime > coreBundleCacheBuiltAt) {
        stale = true;
        console.log(`[bundler] cache stale (disk.mjs mtime ${new Date(mtime).toISOString()} > built ${new Date(coreBundleCacheBuiltAt).toISOString()}) — rebuilding`);
      }
    } catch { /* ignore stat errors, fall through to normal cache check */ }
  }

  if (!forceRefresh && !stale && coreBundleCache && coreBundleCacheCommit === GIT_COMMIT) {
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
  coreBundleCacheBuiltAt = Date.now();
  console.log(`[bundler] cached ${Object.keys(coreFiles).length} core files`);
  return coreFiles;
}

// ─── KidLisp bundle ─────────────────────────────────────────────────

export async function createBundle(pieceName, onProgress = () => {}, nocompress = false, density = null, brotli = false, noboxart = false, keeplabel = false, forceRefresh = false) {
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

  const coreFiles = await getCoreBundle(onProgress, forceRefresh);
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

  const boxArtPNG = noboxart ? null : await generateBoxArtPNG(PIECE_NAME, authorHandle, bgColor, packDate).catch(() => null);

  const htmlContent = generateHTMLBundle({
    PIECE_NAME, PIECE_NAME_NO_DOLLAR, mainSource, kidlispSources,
    files, paintingData, authorHandle, packDate, packTime,
    gitVersion: GIT_COMMIT, filename, density, bgColor, bdfGlyphs, boxArtPNG, keeplabel,
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

export async function createJSPieceBundle(pieceName, onProgress = () => {}, nocompress = false, density = null, brotli = false, noboxart = false, keeplabel = false, forceDaw = false, forceRefresh = false) {
  const acDir = AC_SOURCE_DIR;
  onProgress({ stage: "init", message: `Bundling ${pieceName}...` });

  const packTime = Date.now();
  const packDate = new Date().toLocaleString("en-US", {
    timeZone: "America/Los_Angeles", year: "numeric", month: "long", day: "numeric",
    hour: "numeric", minute: "2-digit", second: "2-digit", hour12: true,
  });
  const bundleTimestamp = timestamp();

  const coreFiles = await getCoreBundle(onProgress, forceRefresh);
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

  const boxArtPNG = noboxart ? null : await generateBoxArtPNG(pieceName, null, null, packDate).catch(() => null);

  const htmlContent = generateJSPieceHTMLBundle({ pieceName, files, packDate, packTime, gitVersion: GIT_COMMIT, bdfGlyphs, boxArtPNG, keeplabel, forceDaw });
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

// ─── JS library bundle (aesthetic.computer.js) ──────────────────────
//
// Same frozen runtime as the HTML pack, but emitted as a standalone
// JavaScript file instead of an HTML document. Including the file with a
// <script> tag (or import) exposes a global `AestheticComputer` whose
// `boot(opts)` runs the exact Phase-1 setup the HTML pack does inline
// (VFS → blob URLs → import map → fetch shim) and then imports boot.mjs.
//
// Unlike the HTML pack, there is no <head>/<body> shell — the consumer's
// page owns the document. boot.mjs still takes over document.body (it
// builds its own wrapper + canvas), so a packjs library boots into the
// page it is loaded by, the same way the HTML pack boots into its own doc.

export async function createJSLibraryBundle(
  pieceName,
  isJSPiece,
  onProgress = () => {},
  density = null,
  forceRefresh = false,
) {
  const bundleTimestamp = timestamp();
  let opts;

  if (isJSPiece) {
    const acDir = AC_SOURCE_DIR;
    onProgress({ stage: "init", message: `Bundling ${pieceName}...` });

    const packTime = Date.now();
    const packDate = new Date().toLocaleString("en-US", {
      timeZone: "America/Los_Angeles", year: "numeric", month: "long", day: "numeric",
      hour: "numeric", minute: "2-digit", second: "2-digit", hour12: true,
    });

    const coreFiles = await getCoreBundle(onProgress, forceRefresh);
    const files = { ...coreFiles };
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

    const bdfGlyphs = files.__bdfGlyphs || {};
    delete files.__bdfGlyphs;

    opts = {
      pieceName, isKidLisp: false, files, bdfGlyphs,
      packDate, packTime, gitVersion: GIT_COMMIT, density,
      mainSource: null, kidlispSources: null, paintingData: null,
      authorHandle: "@jeffrey", bgColor: null,
    };
  } else {
    const PIECE_NAME_NO_DOLLAR = pieceName.replace(/^\$/, "");
    const PIECE_NAME = "$" + PIECE_NAME_NO_DOLLAR;

    onProgress({ stage: "fetch", message: `Fetching $${PIECE_NAME_NO_DOLLAR}...` });
    const { sources: kidlispSources, authorHandle } = await getKidLispSourceWithDeps(PIECE_NAME_NO_DOLLAR);
    const mainSource = kidlispSources[PIECE_NAME_NO_DOLLAR];
    const depCount = Object.keys(kidlispSources).length - 1;
    onProgress({ stage: "deps", message: `Found ${depCount} dependenc${depCount === 1 ? "y" : "ies"}` });

    const packTime = Date.now();
    const packDate = new Date().toLocaleString("en-US", {
      timeZone: "America/Los_Angeles", year: "numeric", month: "long", day: "numeric",
      hour: "numeric", minute: "2-digit", second: "2-digit", hour12: true,
    });

    const coreFiles = await getCoreBundle(onProgress, forceRefresh);
    const files = { ...coreFiles };
    for (const [stubPath, stubContent] of Object.entries(VFS_STUBS)) {
      files[stubPath] = { content: stubContent, binary: false, type: "mjs" };
    }

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

    const bdfGlyphs = files.__bdfGlyphs || {};
    delete files.__bdfGlyphs;

    opts = {
      pieceName: PIECE_NAME, pieceNameNoDollar: PIECE_NAME_NO_DOLLAR, isKidLisp: true,
      files, bdfGlyphs, packDate, packTime, gitVersion: GIT_COMMIT, density,
      mainSource, kidlispSources, paintingData,
      authorHandle, bgColor: extractFirstColor(mainSource),
    };
  }

  onProgress({ stage: "generate", message: "Generating aesthetic.computer.js..." });
  const js = generateJSLibrary(opts);

  const safeName = (isJSPiece ? pieceName : pieceName.replace(/^\$/, "$")).replace(/[^a-zA-Z0-9$_-]/g, "_");
  const filename = `aesthetic.computer.js`;
  void safeName; void bundleTimestamp;

  return { js, filename, sizeKB: Math.round(js.length / 1024) };
}

function generateJSLibrary(opts) {
  const {
    pieceName, pieceNameNoDollar, isKidLisp, files, bdfGlyphs,
    packDate, packTime, gitVersion, density,
    mainSource, kidlispSources, paintingData, authorHandle, bgColor,
  } = opts;

  // Serialize VFS / glyphs once; close </script> just in case the file is
  // ever inlined into an HTML <script> tag by a consumer.
  const vfsJSON = JSON.stringify(files).replace(/<\/script>/g, "<\\/script>");
  const glyphsJSON = JSON.stringify(bdfGlyphs);
  const colophon = isKidLisp
    ? {
        piece: { name: pieceNameNoDollar, sourceCode: mainSource, isKidLisp: true },
        build: { author: authorHandle, packTime, gitCommit: gitVersion, gitIsDirty: false, fileCount: Object.keys(files).length },
      }
    : {
        piece: { name: pieceName, isKidLisp: false },
        build: { author: authorHandle || "@jeffrey", packTime, gitCommit: gitVersion, gitIsDirty: false, fileCount: Object.keys(files).length },
      };

  // The smart-density snippet matches the HTML pack so a packjs library
  // picks a sensible resolution when the caller doesn't pass `density`.
  const densitySnippet = density
    ? `window.acPACK_DENSITY = ${density};`
    : `if (window.acPACK_DENSITY === undefined) { var sw = window.screen.width * (window.devicePixelRatio || 1); var sh = window.screen.height * (window.devicePixelRatio || 1); var maxDim = Math.max(sw, sh); if (maxDim >= 3840) { window.acPACK_DENSITY = 8; } else if (Math.abs(sw - sh) < 100 && maxDim >= 1000 && maxDim <= 1200) { window.acPACK_DENSITY = 4; } else { window.acPACK_DENSITY = 3; } }`;

  return `/*! aesthetic.computer.js — frozen AC runtime library
 *  piece: ${pieceName}   packed: ${packDate}   commit: ${gitVersion}
 *
 *  Usage (script tag):
 *    <script src="aesthetic.computer.js"></script>
 *    <script>AestheticComputer.boot();</script>
 *
 *  Usage (module):
 *    import AC from "./aesthetic.computer.js"; AC.boot();
 *
 *  boot(opts?) options:
 *    piece   — override the embedded starting piece (default: ${pieceName})
 *    density — pixel density override (default: smart)
 *  Returns a Promise that resolves once boot.mjs has been imported.
 *  Note: the runtime takes over document.body, like the HTML pack.
 */
(function (root, factory) {
  var AC = factory();
  if (typeof module === "object" && module.exports) module.exports = AC;
  root.AestheticComputer = AC;
})(typeof self !== "undefined" ? self : this, function () {
  "use strict";

  var VFS = ${vfsJSON};
  var GLYPHS = ${glyphsJSON};
  var DEFAULT_PIECE = ${JSON.stringify(pieceName)};
  var KIDLISP_SOURCE = ${isKidLisp ? JSON.stringify(mainSource) : "null"};
  var KIDLISP_SOURCES = ${isKidLisp ? JSON.stringify(kidlispSources) : "null"};
  var PAINTING_CODE_MAP = ${isKidLisp ? JSON.stringify(paintingData) : "{}"};
  var COLOPHON = ${JSON.stringify(colophon)};
  var IS_KIDLISP = ${isKidLisp ? "true" : "false"};
  var BG_COLOR = ${JSON.stringify(bgColor || "black")};

  var booted = false;

  function decodePaintingToBitmap(base64Data) {
    return new Promise(function (resolve, reject) {
      var img = new Image();
      img.onload = function () {
        var c = document.createElement("canvas");
        c.width = img.width; c.height = img.height;
        var ctx = c.getContext("2d");
        ctx.drawImage(img, 0, 0);
        var d = ctx.getImageData(0, 0, c.width, c.height);
        resolve({ width: d.width, height: d.height, pixels: d.data });
      };
      img.onerror = reject;
      img.src = "data:image/png;base64," + base64Data;
    });
  }

  function setupPhase1(piece) {
    // ── Globals (mirror the HTML pack's Phase-1 block) ───────────────
    window.acPACK_MODE = true;
    window.acUseWebGLComposite = false;
    window.KIDLISP_SUPPRESS_SNAPSHOT_LOGS = true;
    window.__acKidlispConsoleEnabled = false;
    window.acKEEP_MODE = IS_KIDLISP;
    window.acSTARTING_PIECE = piece;
    window.acPACK_PIECE = piece;
    window.acPACK_DATE = ${JSON.stringify(packDate)};
    window.acPACK_GIT = ${JSON.stringify(gitVersion)};
    window.acPACK_COLOPHON = COLOPHON;
    window.acBUNDLED_GLYPHS = GLYPHS;
    window.acOBJKT_MATRIX_CHUNKY_GLYPHS = GLYPHS.MatrixChunky8 || {};
    window.VFS = VFS;
    ${densitySnippet}

    if (IS_KIDLISP) {
      window.acKIDLISP_SOURCE = KIDLISP_SOURCE;
      window.EMBEDDED_KIDLISP_SOURCE = KIDLISP_SOURCE;
      window.EMBEDDED_KIDLISP_PIECE = ${JSON.stringify(pieceNameNoDollar || "")};
      window.objktKidlispCodes = KIDLISP_SOURCES;
      window.acPREFILL_CODE_CACHE = KIDLISP_SOURCES;
      window.acPAINTING_CODE_MAP = PAINTING_CODE_MAP;
      window.acEMBEDDED_PAINTING_BITMAPS = {};
      window.acPAINTING_BITMAPS_READY = false;
    }

    // ── Block live CSS/font <link> injection ─────────────────────────
    var origAppend = Element.prototype.appendChild;
    Element.prototype.appendChild = function (child) {
      if (child.tagName === "LINK" && child.rel === "stylesheet" && child.href && child.href.includes(".css")) return child;
      return origAppend.call(this, child);
    };
    var origBodyAppend = HTMLBodyElement.prototype.append;
    HTMLBodyElement.prototype.append = function () {
      var args = []; for (var i = 0; i < arguments.length; i++) { var n = arguments[i]; if (!(n.tagName === "LINK" && n.rel === "stylesheet")) args.push(n); }
      return origBodyAppend.apply(this, args);
    };

    // ── VFS → blob URLs ──────────────────────────────────────────────
    window.VFS_BLOB_URLS = {};
    window.modulePaths = [];
    Object.entries(window.VFS).forEach(function (entry) {
      var p = entry[0], file = entry[1];
      if (p.endsWith(".mjs") || p.endsWith(".js")) {
        var blob = new Blob([file.content], { type: "application/javascript" });
        window.VFS_BLOB_URLS[p] = URL.createObjectURL(blob);
        window.modulePaths.push(p);
      }
    });

    // ── Import map (must be in the DOM before the first module import) ─
    var entries = {};
    for (var i = 0; i < window.modulePaths.length; i++) {
      var fp = window.modulePaths[i];
      if (window.VFS_BLOB_URLS[fp]) {
        entries[fp] = window.VFS_BLOB_URLS[fp];
        entries["/" + fp] = window.VFS_BLOB_URLS[fp];
        entries["./" + fp] = window.VFS_BLOB_URLS[fp];
        entries["../" + fp] = window.VFS_BLOB_URLS[fp];
        entries["../../" + fp] = window.VFS_BLOB_URLS[fp];
        entries["aesthetic.computer/" + fp] = window.VFS_BLOB_URLS[fp];
        entries["/aesthetic.computer/" + fp] = window.VFS_BLOB_URLS[fp];
        entries["./aesthetic.computer/" + fp] = window.VFS_BLOB_URLS[fp];
        entries["https://aesthetic.computer/" + fp] = window.VFS_BLOB_URLS[fp];
      }
    }
    var imScript = document.createElement("script");
    imScript.type = "importmap";
    imScript.textContent = JSON.stringify({ imports: entries });
    document.head.appendChild(imScript);

    // ── fetch shim: serve the VFS / glyphs / paintings, neuter /api/ ─
    var origFetch = window.fetch;
    window.fetch = function (url, options) {
      var urlStr = typeof url === "string" ? url : url.toString();
      if (urlStr.includes("/api/")) {
        if (urlStr.includes("/api/bdf-glyph") && window.acBUNDLED_GLYPHS) {
          var fontM = urlStr.match(/[?&]font=([^&]+)/);
          var charsM = urlStr.match(/[?&]chars?=([^&]+)/);
          var fontKey = fontM ? decodeURIComponent(fontM[1]) : "unifont";
          if (fontKey === "unifont-16.0.03") fontKey = "unifont";
          var fontMap = window.acBUNDLED_GLYPHS[fontKey] || {};
          var glyphs = {};
          if (charsM) { for (var c of charsM[1].split(",")) { var hex = c.trim().toUpperCase(); if (fontMap[hex]) glyphs[c.trim()] = fontMap[hex]; } }
          return Promise.resolve(new Response(JSON.stringify({ glyphs: glyphs }), { status: 200, headers: { "Content-Type": "application/json" } }));
        }
        if (urlStr.includes("/api/painting-code")) {
          var m = urlStr.match(/[?&]code=([^&]+)/);
          if (m) { var info = window.acPAINTING_CODE_MAP && window.acPAINTING_CODE_MAP[m[1]]; if (info) return Promise.resolve(new Response(JSON.stringify({ code: info.code, handle: info.handle, slug: info.slug }), { status: 200, headers: { "Content-Type": "application/json" } })); }
          return Promise.resolve(new Response(JSON.stringify({ error: "Not found" }), { status: 404 }));
        }
        return Promise.resolve(new Response("{}", { status: 200, headers: { "Content-Type": "application/json" } }));
      }
      var vfsPath = decodeURIComponent(urlStr).replace(/^https?:\\/\\/[^\\/]+\\//g, "").replace(/^aesthetic\\.computer\\//g, "").replace(/#.*$/g, "").replace(/\\?.*$/g, "");
      vfsPath = vfsPath.replace(/^\\.\\.\\/+/g, "").replace(/^\\.\\//g, "").replace(/^\\//g, "").replace(/^aesthetic\\.computer\\//g, "");
      if (IS_KIDLISP && urlStr.includes("/media/") && urlStr.includes("/painting/")) {
        var pk = Object.keys(window.acPAINTING_CODE_MAP || {});
        for (var pi = 0; pi < pk.length; pi++) {
          var pcode = pk[pi], pinfo = window.acPAINTING_CODE_MAP[pcode];
          if (urlStr.includes(pinfo.slug)) {
            var pvfs = "paintings/" + pcode + ".png";
            if (window.VFS[pvfs]) { var f0 = window.VFS[pvfs]; var b0 = atob(f0.content); var u0 = new Uint8Array(b0.length); for (var z0 = 0; z0 < b0.length; z0++) u0[z0] = b0.charCodeAt(z0); return Promise.resolve(new Response(u0, { status: 200, headers: { "Content-Type": "image/png" } })); }
          }
        }
      }
      if (window.VFS[vfsPath]) {
        var file = window.VFS[vfsPath]; var content; var ct = "text/plain";
        if (file.binary) { var bs = atob(file.content); var bytes = new Uint8Array(bs.length); for (var j = 0; j < bs.length; j++) bytes[j] = bs.charCodeAt(j); content = bytes; if (file.type === "png") ct = "image/png"; else if (file.type === "jpg" || file.type === "jpeg") ct = "image/jpeg"; }
        else { content = file.content; if (file.type === "mjs" || file.type === "js") ct = "application/javascript"; else if (file.type === "json") ct = "application/json"; }
        return Promise.resolve(new Response(content, { status: 200, headers: { "Content-Type": ct } }));
      }
      if (vfsPath.includes("disks/drawings/font_") || vfsPath.endsWith(".mjs") || vfsPath.includes("cursors/") || vfsPath.endsWith(".svg") || vfsPath.endsWith(".css") || urlStr.includes("/type/webfonts/")) {
        return Promise.resolve(new Response("", { status: 200, headers: { "Content-Type": "text/css" } }));
      }
      return origFetch.call(this, url, options);
    };

    // ── Decode embedded paintings (KidLisp only) ─────────────────────
    if (IS_KIDLISP) {
      var promises = [];
      var map = window.acPAINTING_CODE_MAP || {};
      Object.keys(map).forEach(function (code) {
        var vfsPath2 = "paintings/" + code + ".png";
        if (window.VFS[vfsPath2]) {
          promises.push(decodePaintingToBitmap(window.VFS[vfsPath2].content).then(function (bitmap) {
            window.acEMBEDDED_PAINTING_BITMAPS["#" + code] = bitmap;
            window.acEMBEDDED_PAINTING_BITMAPS[code] = bitmap;
          }).catch(function () {}));
        }
      });
      return Promise.all(promises).then(function () { window.acPAINTING_BITMAPS_READY = true; });
    }
    return Promise.resolve();
  }

  function boot(o) {
    o = o || {};
    if (booted) { console.warn("[aesthetic.computer] already booted"); return Promise.resolve(); }
    booted = true;
    if (typeof window === "undefined") return Promise.reject(new Error("AestheticComputer.boot() requires a browser environment"));
    if (o.density != null) window.acPACK_DENSITY = o.density;
    var piece = o.piece || DEFAULT_PIECE;
    document.documentElement.style.background = BG_COLOR;
    return Promise.resolve(setupPhase1(piece)).then(function () {
      return import(window.VFS_BLOB_URLS["boot.mjs"]);
    });
  }

  return {
    boot: boot,
    version: ${JSON.stringify(gitVersion)},
    piece: DEFAULT_PIECE,
    isKidLisp: IS_KIDLISP,
    colophon: COLOPHON,
  };
});
`;
}

// ─── M4D (Max for Live) ─────────────────────────────────────────────

const M4L_HEADER_INSTRUMENT = Buffer.from(
  "ampf\x04\x00\x00\x00iiiimeta\x04\x00\x00\x00\x00\x00\x00\x00ptch", "binary"
);
// MIDI effect header — used by devices that emit MIDI (noteout) without audio
// output, e.g. notepat-remote. Tag is 'mmmm' where instruments use 'iiii'.
// Live refuses to load a 'mmmm' device on an audio track; an 'iiii' device
// on a MIDI track with no audio outlet shows up but can't route MIDI cleanly.
const M4L_HEADER_MIDI_EFFECT = Buffer.from(
  "ampf\x04\x00\x00\x00mmmmmeta\x04\x00\x00\x00\x00\x00\x00\x00ptch", "binary"
);

function generateM4DPatcher(pieceName, dataUri, width = 400, height = 200, pieceProfile = "default") {
  if (pieceProfile === "notepat") return generateNotepatM4DPatcher(pieceName, dataUri);
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

// Notepat → Live recording bridge. [noteout] feeds the device chain
// (downstream instrument plays) but Live records the track INPUT, not
// what an M4L MIDI effect emits — so without LiveAPI the played notes
// are heard but never captured. This v8.codebox catches the same
// notedown/noteup stream and writes notes into the currently-recording
// session clip on the device's parent track via add_new_notes.
const NOTEPAT_RECORDER_JS = `autowatch = 0;
inlets = 1;
outlets = 1;

const DEFAULT_VELOCITY = 100;
const MIN_DURATION = 1 / 32;
// Generous default so a few bars of playing fits without needing the
// auto-extender to kick in mid-take. Extender below grows it further
// if the user keeps going past this length.
const AUTO_CLIP_BEATS = 32;
const active = {};
let lastDigest = "";
// Stickied auto-created clip for the current recording session. Reset
// whenever the user disengages record so the next pass starts fresh.
let autoClipId = null;
let prevWantsRecord = false;
let noteCount = 0;
// Time base for our auto-created clips. Live's session-auto clips are
// not actually "playing", so clip.playing_position reads 0 every time
// and all notes end up stacked on beat 0 (which is what was happening:
// every wrote-notes message logged start=0.000). Capture a reference at
// the moment recording engages and translate elapsed wall time into
// beats via the live_set tempo. If transport is playing when record
// engages, prefer current_song_time (sample-accurate, drift-free).
let recordStartedAtMs = 0;
let recordStartSongBeats = 0;
let recordCapturedSongTime = false;

function captureRecordStart() {
  recordStartedAtMs = Date.now();
  const set = new LiveAPI("live_set");
  const playing = parseInt(set.get("is_playing")) === 1;
  recordCapturedSongTime = playing;
  recordStartSongBeats = playing ? parseFloat(set.get("current_song_time")) : 0;
  log("record start captured (playing=" + playing + " songBeats=" + recordStartSongBeats + ")");
}

function elapsedBeats() {
  const set = new LiveAPI("live_set");
  const playing = parseInt(set.get("is_playing")) === 1;
  if (playing && recordCapturedSongTime) {
    const t = parseFloat(set.get("current_song_time")) - recordStartSongBeats;
    return t >= 0 ? t : 0;
  }
  let tempo = parseFloat(set.get("tempo"));
  if (!isFinite(tempo) || tempo <= 0) tempo = 120;
  const elapsedMs = Date.now() - recordStartedAtMs;
  return (elapsedMs / 1000) * (tempo / 60);
}

// Grow the clip's loop_end / end_marker if our recording position has
// reached the current end. Without this the auto-created 32-beat clip
// truncates anything past the boundary.
function ensureClipLength(clip, atLeastBeats) {
  if (!clip || clip.id == 0) return;
  let len = parseFloat(clip.get("length"));
  if (!isFinite(len) || len <= 0) len = AUTO_CLIP_BEATS;
  if (atLeastBeats > len - 0.5) {
    const target = Math.ceil(atLeastBeats + 4);
    try { clip.set("loop_end", target); } catch (_) {}
    try { clip.set("end_marker", target); } catch (_) {}
  }
}

function logOnChange(key, msg) {
  if (key === lastDigest) return;
  lastDigest = key;
  post("[NOTEPAT-REC] " + msg + "\\n");
}
function log(msg) { post("[NOTEPAT-REC] " + msg + "\\n"); }

log("loaded — send 'info' to dump full state, 'transport' to play/pause");

// Dump current state of the device's parent track so we can see why a
// recording attempt isn't landing where expected. Triggered by the
// "info" message into the v8 inlet OR called automatically the first
// time we hit a "no recording target" condition each pass.
function dumpState(prefix) {
  prefix = prefix || "info";
  const track = new LiveAPI("this_device canonical_parent");
  if (!track || track.id == 0) {
    log(prefix + ": no track resolved (this_device canonical_parent)");
    return;
  }
  const trackName = track.get("name");
  log(prefix + ": track id=" + track.id + " name=" + trackName +
      " arm=" + track.get("arm") + " input_routing_type=" + track.get("input_routing_type"));

  const set = new LiveAPI("live_set");
  log(prefix + ": transport playing=" + set.get("is_playing") +
      " session_record=" + set.get("session_record") +
      " record_mode=" + set.get("record_mode") +
      " tempo=" + set.get("tempo"));

  const slotsList = track.get("clip_slots");
  const slotCount = slotsList ? parseInt(slotsList.length / 2) : 0;
  log(prefix + ": " + slotCount + " session slot(s) on track");
  for (let i = 0; i < slotCount; i++) {
    const slot = new LiveAPI("this_device canonical_parent clip_slots " + i);
    if (!slot || slot.id == 0) continue;
    const has = parseInt(slot.get("has_clip"));
    const triggered = parseInt(slot.get("is_triggered"));
    let clipInfo = "empty";
    if (has === 1) {
      const clip = new LiveAPI("this_device canonical_parent clip_slots " + i + " clip");
      if (clip && clip.id != 0) {
        clipInfo = "clip id=" + clip.id +
                   " name=" + clip.get("name") +
                   " is_recording=" + clip.get("is_recording") +
                   " is_playing=" + clip.get("is_playing") +
                   " length=" + clip.get("length");
      }
    }
    log(prefix + ":   slot " + i + " has_clip=" + has + " triggered=" + triggered + " " + clipInfo);
  }

  const arrClips = track.get("arrangement_clips");
  const arrCount = arrClips ? parseInt(arrClips.length / 2) : 0;
  log(prefix + ": " + arrCount + " arrangement clip(s) on track");
  for (let i = 1; arrClips && i < arrClips.length; i += 2) {
    const arr = new LiveAPI("id " + arrClips[i]);
    if (!arr || arr.id == 0) continue;
    log(prefix + ":   arr clip id=" + arr.id +
        " name=" + arr.get("name") +
        " is_recording=" + arr.get("is_recording") +
        " start=" + arr.get("start_time") +
        " length=" + arr.get("length"));
  }
}

function info() { dumpState("info"); }
function bang() { dumpState("info"); }

// Spacebar pass-through from jweb → toggle Live transport. Mirrors the
// host app's spacebar behavior so the user doesn't have to alt-tab out
// of the device panel to control playback.
function transport() {
  const set = new LiveAPI("live_set");
  if (!set || set.id == 0) { log("transport: no live_set"); return; }
  const playing = parseInt(set.get("is_playing")) === 1;
  if (playing) {
    set.call("stop_playing");
    log("transport: stop");
  } else {
    set.call("start_playing");
    log("transport: start");
  }
}

// Find or create a clip to write notes into. Priority order:
//   1. A session clip with is_recording == 1 (Live's active session record)
//   2. An arrangement clip with is_recording == 1
//   3. Auto-create OR reuse a single session clip when the user has
//      engaged a real record state (session_record on, OR arrangement
//      record + playing). The clip is named "notepat" and stays sticky
//      for the rest of that recording pass — we don't create one per
//      keypress. Resets when record disengages.
//   4. Bail.
//
// Track-armed-alone is intentionally NOT a trigger: arming is monitor
// intent, not record intent. Without this gate every notedown on an
// armed track without record would auto-create a fresh clip.
function recordingClip() {
  const track = new LiveAPI("this_device canonical_parent");
  if (!track || track.id == 0) {
    return { failed: true, digest: "no-track" };
  }

  // 1. Active session-record slot — Live is actually recording into it.
  const slotsList = track.get("clip_slots");
  const slotCount = slotsList ? parseInt(slotsList.length / 2) : 0;
  let firstEmptySlot = -1;
  for (let i = 0; i < slotCount; i++) {
    const slot = new LiveAPI("this_device canonical_parent clip_slots " + i);
    if (!slot || slot.id == 0) continue;
    if (parseInt(slot.get("has_clip")) !== 1) {
      if (firstEmptySlot < 0) firstEmptySlot = i;
      continue;
    }
    const clip = new LiveAPI("this_device canonical_parent clip_slots " + i + " clip");
    if (!clip || clip.id == 0) continue;
    if (parseInt(clip.get("is_recording")) === 1) {
      return { clip: clip, kind: "session-rec", digest: "sess-rec:" + clip.id };
    }
  }

  const set = new LiveAPI("live_set");
  const arrRec = parseInt(set.get("record_mode")) === 1;
  const sessRec = parseInt(set.get("session_record")) === 1;
  const playing = parseInt(set.get("is_playing")) === 1;

  // 2. Active arrangement-record clip
  if (arrRec && playing) {
    const arrClips = track.get("arrangement_clips");
    for (let i = 1; arrClips && i < arrClips.length; i += 2) {
      const arrClip = new LiveAPI("id " + arrClips[i]);
      if (!arrClip || arrClip.id == 0) continue;
      if (parseInt(arrClip.get("is_recording")) === 1) {
        return { clip: arrClip, kind: "arr-rec", digest: "arr-rec:" + arrClip.id };
      }
    }
  }

  // 3. wantsRecord gate — only proceed if the user has engaged any
  //    record intent. Track-armed-alone is not enough (arming is
  //    monitor intent; without this gate every keystroke on an armed
  //    track without record would create clips).
  const wantsRecord = sessRec || (arrRec && playing);
  if (!wantsRecord) {
    if (prevWantsRecord) {
      log("record disengaged — sticky clip will reset on next pass");
      prevWantsRecord = false;
      autoClipId = null;
    }
    const digest = "idle|sessRec=0|arrRec=" + (arrRec ? 1 : 0) + "|play=" + (playing ? 1 : 0);
    return { failed: true, digest: digest };
  }

  // Fresh recording pass — capture the time base, clear sticky clip.
  if (!prevWantsRecord) {
    autoClipId = null;
    prevWantsRecord = true;
    captureRecordStart();
  }

  // Sticky during the pass — once we lock onto a clip, stay there even
  // if Live's selection changes. Avoids notes hopping mid-take.
  if (autoClipId) {
    const existing = new LiveAPI("id " + autoClipId);
    if (existing && existing.id != 0) {
      return { clip: existing, kind: "session-locked", digest: "lock:" + autoClipId };
    }
    autoClipId = null;
  }

  // 4. First note of the pass — prefer the user's selected clip slot.
  //    This is the slot the user "hit record on" or last clicked in
  //    Session view. If it has a clip, write into it (overdub-style);
  //    if empty, create a clip there. Far more predictable than
  //    auto-allocating a fresh scene.
  const sel = new LiveAPI("this_device canonical_parent view selected_clip_slot");
  if (sel && sel.id != 0) {
    const selHas = parseInt(sel.get("has_clip"));
    if (selHas === 1) {
      const selClip = new LiveAPI("this_device canonical_parent view selected_clip_slot clip");
      if (selClip && selClip.id != 0) {
        autoClipId = selClip.id;
        log("locked onto selected clip id=" + selClip.id + " name=" + selClip.get("name"));
        return { clip: selClip, kind: "session-selected", digest: "sel:" + selClip.id };
      }
    } else {
      try { sel.call("create_clip", AUTO_CLIP_BEATS); } catch (_) {}
      const newClip = new LiveAPI("this_device canonical_parent view selected_clip_slot clip");
      if (newClip && newClip.id != 0) {
        try { newClip.set("name", "notepat"); } catch (_) {}
        autoClipId = newClip.id;
        log("created clip in selected slot id=" + newClip.id);
        return { clip: newClip, kind: "session-selected-new", digest: "sel-new:" + newClip.id };
      }
    }
  }

  // 5. No usable selected slot — fall back to first empty slot or new scene.
  if (firstEmptySlot < 0) {
    log("no empty slot on track — appending a new scene");
    const liveSet = new LiveAPI("live_set");
    try {
      liveSet.call("create_scene", -1);
    } catch (err) {
      logOnChange("scene-fail", "create_scene failed: " + err);
      return { failed: true, digest: "scene-fail" };
    }
    firstEmptySlot = slotCount;
  }
  const slot = new LiveAPI("this_device canonical_parent clip_slots " + firstEmptySlot);
  try {
    slot.call("create_clip", AUTO_CLIP_BEATS);
  } catch (err) {
    logOnChange("create-fail", "create_clip failed in slot " + firstEmptySlot + ": " + err);
    return { failed: true, digest: "create-fail" };
  }
  const clip = new LiveAPI("this_device canonical_parent clip_slots " + firstEmptySlot + " clip");
  if (!clip || clip.id == 0) {
    return { failed: true, digest: "create-no-clip" };
  }
  try { clip.set("name", "notepat"); } catch (_) {}
  autoClipId = clip.id;
  log("auto-created notepat clip in slot " + firstEmptySlot + " (" + AUTO_CLIP_BEATS + " beats) — sticky for this pass");
  return { clip: clip, kind: "session-auto", digest: "sess-auto:" + autoClipId };
}

function clipById(id) {
  if (!id) return null;
  const api = new LiveAPI("id " + id);
  if (!api || api.id == 0) return null;
  return api;
}

function clipPos(clip) {
  return parseFloat(clip.get("playing_position"));
}

function startNote(pitch, velocity) {
  if (typeof pitch !== "number") return;
  noteCount++;
  const dest = recordingClip();
  if (!dest || dest.failed) {
    const digest = dest ? dest.digest : "null";
    if (digest !== lastDigest) {
      logOnChange(digest, "no recording target — " + digest + " (note #" + noteCount + ", pitch=" + pitch + ")");
      dumpState("miss");
    }
    return;
  }
  logOnChange(dest.digest, "writing into " + dest.kind + " clip=" + dest.clip.id);
  // Live-managed recording clips (session-rec / arr-rec) advance their
  // own playing_position; trust it. For everything else (selected /
  // locked / auto), playing_position stays at 0 unless the clip is
  // actually playing — fall back to our own elapsed-beats clock.
  const useLiveClock = dest.kind === "session-rec" || dest.kind === "arr-rec";
  const start = useLiveClock ? clipPos(dest.clip) : elapsedBeats();
  active[pitch] = {
    startInClip: isFinite(start) ? start : 0,
    velocity: velocity,
    clipId: dest.clip.id,
    useLiveClock: useLiveClock,
  };
}

function endNote(pitch) {
  if (typeof pitch !== "number") return;
  const note = active[pitch];
  if (!note) return;
  delete active[pitch];
  const clip = clipById(note.clipId);
  if (!clip) {
    log("noteoff pitch=" + pitch + ": clip " + note.clipId + " gone");
    return;
  }
  const endPos = note.useLiveClock ? clipPos(clip) : elapsedBeats();
  let duration = endPos - note.startInClip;
  if (!isFinite(duration) || duration < MIN_DURATION) duration = MIN_DURATION;
  // Grow the clip's loop_end / end_marker if the user has played past
  // the current clip length. Without this anything beyond AUTO_CLIP_BEATS
  // gets clipped off and looks invisible in the clip view.
  if (!note.useLiveClock) ensureClipLength(clip, note.startInClip + duration);
  try {
    clip.call("add_new_notes", {
      notes: [{
        pitch: pitch,
        start_time: note.startInClip,
        duration: duration,
        velocity: note.velocity,
        mute: 0,
      }],
    });
    logOnChange("write:" + note.clipId, "wrote notes into clip=" + note.clipId +
      " (most recent: pitch=" + pitch + " start=" + note.startInClip.toFixed(3) +
      " dur=" + duration.toFixed(3) + " vel=" + note.velocity + ")");
  } catch (err) {
    log("add_new_notes failed pitch=" + pitch + " clip=" + note.clipId + ": " + err);
  }
}

function notedown(p) { startNote(p, DEFAULT_VELOCITY); }
function noteup(p)   { endNote(p); }
function note(p, v) {
  if (v > 0) startNote(p, v);
  else endNote(p);
}
`;

// Notepat-specific patcher: routes `notedown`/`noteup`/`octave`/`ping` from
// jweb~ (emitted by bios.mjs's daw bridge) into [noteout], and echoes the
// RTT pong back via script→window.acMaxPong. Mirrors the hand-rolled
// system/public/m4l/notepat-remote.amxd so the offline bundle behaves
// identically to the live version.
function generateNotepatM4DPatcher(pieceName, dataUri) {
  const W = 180, H = 169;
  return {
    patcher: {
      fileversion: 1,
      appversion: { major: 9, minor: 0, revision: 7, architecture: "x64", modernui: 1 },
      classnamespace: "box",
      rect: [100, 100, 900, 520],
      openrect: [0, 0, W, H],
      openinpresentation: 1,
      gridsize: [15, 15],
      enablehscroll: 0, enablevscroll: 0,
      devicewidth: W,
      description: `Aesthetic Computer ${pieceName} (offline) — packed notepat + local hotkey input. BIOS in jweb~ owns keyboard + octave state and emits pitches via window.max.outlet (notedown/noteup).`,
      boxes: [
        { box: { disablefind: 0, id: "obj-jweb", latency: 0, maxclass: "jweb~", numinlets: 1, numoutlets: 3, outlettype: ["signal","signal",""], patching_rect: [10,10,W,H], presentation: 1, presentation_rect: [0,0,W+1,H+1], rendermode: 1, url: dataUri } },
        { box: { id: "obj-route", maxclass: "newobj", numinlets: 1, numoutlets: 7, outlettype: ["","","","","","",""], patching_rect: [10,250,560,22], text: "route note channel notedown noteup octave focus ping" } },
        { box: { id: "obj-noteout", maxclass: "newobj", numinlets: 2, numoutlets: 0, patching_rect: [10,400,60,22], text: "noteout" } },
        { box: { id: "obj-pack-on", maxclass: "newobj", numinlets: 2, numoutlets: 1, outlettype: ["list"], patching_rect: [10,360,90,22], text: "pack 0 100" } },
        { box: { id: "obj-pack-off", maxclass: "newobj", numinlets: 2, numoutlets: 1, outlettype: ["list"], patching_rect: [120,360,90,22], text: "pack 0 0" } },
        { box: { id: "obj-print-note", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [600,290,200,22], text: "print NOTEPAT-NOTE" } },
        { box: { id: "obj-print-chan", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [600,320,200,22], text: "print NOTEPAT-CHAN" } },
        { box: { id: "obj-print-keydown", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [600,360,200,22], text: "print NOTEPAT-DOWN" } },
        { box: { id: "obj-print-keyup", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [600,390,200,22], text: "print NOTEPAT-UP" } },
        { box: { id: "obj-print-octave", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [600,420,200,22], text: "print NOTEPAT-OCT" } },
        { box: { id: "obj-print-focus", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [600,450,200,22], text: "print NOTEPAT-FOCUS" } },
        { box: { id: "obj-print-other", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [600,480,200,22], text: "print NOTEPAT-OTHER" } },
        { box: { id: "obj-sprintf-pong", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [10,290,320,22], text: "sprintf script window.acMaxPong(%ld)" } },
        { box: { id: "obj-thisdevice", maxclass: "newobj", numinlets: 1, numoutlets: 3, outlettype: ["bang","int","int"], patching_rect: [240,290,90,22], text: "live.thisdevice" } },
        { box: { id: "obj-routeready", maxclass: "newobj", numinlets: 1, numoutlets: 2, outlettype: ["",""], patching_rect: [240,320,60,22], text: "route ready" } },
        { box: { id: "obj-activate", maxclass: "message", numinlets: 2, numoutlets: 1, outlettype: [""], patching_rect: [240,350,120,22], text: "script daw-activate" } },
        // Console → Max log routing. jweb's 3rd outlet emits any dict/list
        // from window.max.outlet, including "log"/"error"/"warn" calls wired
        // by bios.mjs. [route log error warn] peels them off the MIDI stream
        // into a Max console printer so we can see bundle errors in the
        // Max Console window when the UI goes gray.
        { box: { id: "obj-route-logs", maxclass: "newobj", numinlets: 1, numoutlets: 4, outlettype: ["","","",""], patching_rect: [340,220,160,22], text: "route log error warn" } },
        { box: { id: "obj-print-log", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [340,250,200,22], text: "print [AC-LOG]" } },
        { box: { id: "obj-print-error", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [450,250,200,22], text: "print [AC-ERROR]" } },
        { box: { id: "obj-print-warn", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [560,250,200,22], text: "print [AC-WARN]" } },
        // Live recording bridge — taps the same notedown/noteup stream
        // and writes notes into the recording clip via LiveAPI.
        { box: { id: "obj-prepend-note-rec", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [10,440,90,22], text: "prepend note" } },
        { box: { id: "obj-prepend-down-rec", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [110,440,110,22], text: "prepend notedown" } },
        { box: { id: "obj-prepend-up-rec", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [230,440,100,22], text: "prepend noteup" } },
        { box: { id: "obj-recorder", maxclass: "v8.codebox", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [10,480,720,180], filename: "none", saved_object_attributes: { parameter_enable: 0 }, code: NOTEPAT_RECORDER_JS } },
      ],
      lines: [
        { patchline: { source: ["obj-jweb", 2], destination: ["obj-route", 0] } },
        { patchline: { source: ["obj-jweb", 2], destination: ["obj-route-logs", 0] } },
        { patchline: { source: ["obj-route", 0], destination: ["obj-noteout", 0] } },
        { patchline: { source: ["obj-route", 0], destination: ["obj-print-note", 0] } },
        { patchline: { source: ["obj-route", 1], destination: ["obj-noteout", 1] } },
        { patchline: { source: ["obj-route", 1], destination: ["obj-print-chan", 0] } },
        { patchline: { source: ["obj-route", 2], destination: ["obj-pack-on", 0] } },
        { patchline: { source: ["obj-route", 2], destination: ["obj-print-keydown", 0] } },
        { patchline: { source: ["obj-route", 3], destination: ["obj-pack-off", 0] } },
        { patchline: { source: ["obj-route", 3], destination: ["obj-print-keyup", 0] } },
        { patchline: { source: ["obj-route", 4], destination: ["obj-print-octave", 0] } },
        { patchline: { source: ["obj-route", 5], destination: ["obj-print-focus", 0] } },
        { patchline: { source: ["obj-route", 6], destination: ["obj-sprintf-pong", 0] } },
        { patchline: { source: ["obj-sprintf-pong", 0], destination: ["obj-jweb", 0] } },
        { patchline: { source: ["obj-pack-on", 0], destination: ["obj-noteout", 0] } },
        { patchline: { source: ["obj-pack-off", 0], destination: ["obj-noteout", 0] } },
        { patchline: { source: ["obj-thisdevice", 0], destination: ["obj-routeready", 0] } },
        { patchline: { source: ["obj-routeready", 0], destination: ["obj-activate", 0] } },
        { patchline: { source: ["obj-activate", 0], destination: ["obj-jweb", 0] } },
        { patchline: { source: ["obj-route-logs", 0], destination: ["obj-print-log", 0] } },
        { patchline: { source: ["obj-route-logs", 1], destination: ["obj-print-error", 0] } },
        { patchline: { source: ["obj-route-logs", 2], destination: ["obj-print-warn", 0] } },
        { patchline: { source: ["obj-route", 0], destination: ["obj-prepend-note-rec", 0] } },
        { patchline: { source: ["obj-route", 2], destination: ["obj-prepend-down-rec", 0] } },
        { patchline: { source: ["obj-route", 3], destination: ["obj-prepend-up-rec", 0] } },
        { patchline: { source: ["obj-prepend-note-rec", 0], destination: ["obj-recorder", 0] } },
        { patchline: { source: ["obj-prepend-down-rec", 0], destination: ["obj-recorder", 0] } },
        { patchline: { source: ["obj-prepend-up-rec", 0], destination: ["obj-recorder", 0] } },
      ],
      dependency_cache: [], latency: 0, is_mpe: 0,
      external_mpe_tuning_enabled: 0, minimum_live_version: "",
      minimum_max_version: "", platform_compatibility: 0, autosave: 0,
    },
  };
}

function packAMXD(patcher, kind = "instrument") {
  const json = Buffer.from(JSON.stringify(patcher));
  const len = Buffer.alloc(4);
  len.writeUInt32LE(json.length, 0);
  const header = kind === "midi_effect" ? M4L_HEADER_MIDI_EFFECT : M4L_HEADER_INSTRUMENT;
  return Buffer.concat([header, len, json]);
}

// ─── Chunked bootstrap (for amxd bundles > 32 KB) ───────────────────
//
// Max 9 caps jweb~'s `url` attribute (and message box text) at ~32 KB, so
// we can't ship the full AC runtime as a single data: URI. The chunked
// bootstrap pattern works around this:
//
// 1. The amxd sets `url` to a ~3 KB bootstrap HTML (fits in the attr cap).
// 2. Bootstrap polls for `window.max`, then calls `outlet("ready", 1)`.
// 3. Patcher's `[route ready …]` fires the "ready" signal into N message
//    boxes, each holding one ~28 KB chunk of `executejavascript
//    window._ac.p('I|N|<base64-gz-slice>')`.
// 4. Each chunk message goes to jweb → the JS runs → _ac.p accumulates.
// 5. On the final chunk, _ac.p base64-decodes → gunzips via
//    DecompressionStream → `document.open/write/close` renders the real
//    bundle.
//
// Handshake via "ready" avoids the race where chunks are sent before jweb
// has loaded the page (executejavascript silently drops in that state).

// Max 9 caps message box `text` at ~32 KB. Wrapper is
// `executejavascript window._ac.p('I|N|')` + closing `')` — ~42 chars.
// 28 KB per chunk leaves ample headroom for the wrapper + ints.
const M4D_CHUNK_SIZE = 28000;

function generateChunkedBootstrapHTML(pieceName) {
  const title = pieceName.replace(/[<>&"']/g, "");
  // Compact JS so the whole document stays well under the 24 KB ceiling
  // the `url` data: URI attribute can hold.
  // Online-first: on boot the bootstrap probes aesthetic.computer. On
  // success it tells Max to navigate jweb~ to the live URL (via a
  // `goonline` message); on failure it emits `ready` and lets the chunk
  // messages reassemble the embedded offline bundle.
  return `<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><title>${title}</title>
<style>html,body{margin:0;padding:0;width:100%;height:100%;background:#000;overflow:hidden}pre{display:none}</style>
</head><body><pre id="s"></pre><script>
(function(){
var el=document.getElementById("s");
function log(m){if(el)el.textContent=(el.textContent+"\\n"+m).slice(-3000);try{if(window.max&&window.max.outlet)window.max.outlet("log",String(m).slice(0,900));}catch(_){}}
window._ac={chunks:[],total:0,received:0,
p:function(s){var parts=s.split("|");var i=+parts[0];var n=+parts[1];var d=parts.slice(2).join("|");this.total=n;this.chunks[i]=d;this.received++;if(el)el.textContent="loading "+this.received+"/"+n;if(this.received===n)this.render();},
render:function(){log("all "+this.total+" chunks received, decompressing");try{
var b64=this.chunks.join("");var bin=atob(b64);var bytes=new Uint8Array(bin.length);for(var j=0;j<bin.length;j++)bytes[j]=bin.charCodeAt(j);
var blob=new Blob([bytes],{type:"application/gzip"});
fetch(URL.createObjectURL(blob)).then(function(r){return r.blob();}).then(function(b){return b.stream().pipeThrough(new DecompressionStream("gzip"));}).then(function(s){return new Response(s).text();}).then(function(h){log("decompressed, writing "+h.length+" bytes");document.open();document.write(h);document.close();}).catch(function(e){log("decompress failed: "+e);});
}catch(e){log("render failed: "+e);}}};
window.addEventListener("error",function(e){log("[err] "+e.message);});
window.addEventListener("unhandledrejection",function(e){log("[rej] "+(e.reason&&e.reason.message||e.reason));});
function probe(){return new Promise(function(res){if(typeof navigator!=="undefined"&&navigator.onLine===false){return res(false);}var done=false;var to=setTimeout(function(){if(!done){done=true;res(false);}},2500);try{fetch("https://aesthetic.computer/favicon.ico",{mode:"no-cors",cache:"no-store"}).then(function(){if(!done){done=true;clearTimeout(to);res(true);}}).catch(function(){if(!done){done=true;clearTimeout(to);res(false);}});}catch(e){if(!done){done=true;clearTimeout(to);res(false);}}});}
log("alive, polling for window.max");
var tries=0;var iv=setInterval(function(){if(window.max&&typeof window.max.outlet==="function"){clearInterval(iv);log("window.max bound ("+tries+" tries), probing network");probe().then(function(online){if(online){log("online — sending goonline");try{window.max.outlet("goonline",1);}catch(e){log("goonline send failed: "+e);}}else{log("offline — sending ready");try{window.max.outlet("ready",1);}catch(e){log("ready send failed: "+e);}}});}else if(++tries>100){clearInterval(iv);log("gave up waiting for window.max");}},50);
})();
</script></body></html>`;
}

function chunkBundleForM4D(html) {
  const gz = gzipSync(Buffer.from(html, "utf-8"), { level: 9 });
  const b64 = gz.toString("base64");
  const chunks = [];
  for (let i = 0; i < b64.length; i += M4D_CHUNK_SIZE) {
    chunks.push(b64.slice(i, i + M4D_CHUNK_SIZE));
  }
  return { chunks, gzBytes: gz.length, b64Bytes: b64.length };
}

// Notepat patcher for chunked bundles. jweb outlet 2 feeds `route ready log
// error warn …` — the `ready` signal from the bootstrap fires every chunk
// message into jweb's inlet 0, and unmatched messages (notedown, noteup,
// octave, focus, ping from the daw bridge) pass through the final outlet to
// the MIDI router. Everything else matches the hand-rolled notepat device.
function generateChunkedNotepatM4DPatcher(pieceName, bootstrapDataUri, chunks) {
  const W = 150, H = 169;
  // Cache-bust the live URL with the git commit so jweb/service-worker
  // caches can't keep an older piece pinned after a lith deploy.
  // `noboot=1` hides the boot-canvas VHS animation entirely — tiny
  // M4L device panel looks wrong with it running.
  const liveUrl = "https://aesthetic.computer/" + pieceName +
    "?daw=1&nogap=1&density=1&noboot=1&v=" + GIT_COMMIT;
  // presentation_rect gets a +1 on each axis so jweb content bleeds
  // 1px past the visible device rect — this is the well-known Max
  // quirk fix (same as the generic instrument patcher) for hiding
  // the 1px chrome seam that otherwise shows up on the right/bottom.
  const boxes = [
    { box: { disablefind: 0, id: "obj-jweb", latency: 0, maxclass: "jweb~", numinlets: 1, numoutlets: 3, outlettype: ["signal","signal",""], patching_rect: [10,10,W,H], presentation: 1, presentation_rect: [0,0,W+1,H+1], rendermode: 1, url: bootstrapDataUri } },
    // Split jweb outlet 2 first by handshake/log symbols, then pass anything
    // else to the MIDI router. `route` has (N matched + 1 unmatched) outlets.
    // `goonline` means the bootstrap's network probe succeeded — swap
    // jweb~'s URL to the live piece and skip chunk reassembly entirely.
    // `requestTrackColor` is emitted by the piece post-boot to pull the
    // current Live track color (observer's initial fire happens before
    // the piece has mounted its handler). `requestFocus` does the same
    // for the [active] focus signal.
    { box: { id: "obj-route-top", maxclass: "newobj", numinlets: 1, numoutlets: 8, outlettype: ["","","","","","","",""], patching_rect: [10,200,560,22], text: "route ready goonline requestTrackColor requestFocus log error warn" } },
    { box: { id: "obj-goonline-msg", maxclass: "message", numinlets: 2, numoutlets: 1, outlettype: [""], patching_rect: [10,160,560,22], text: "url " + liveUrl } },
    { box: { id: "obj-print-log", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [100,230,200,22], text: "print [AC-LOG]" } },
    { box: { id: "obj-print-error", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [200,230,200,22], text: "print [AC-ERROR]" } },
    { box: { id: "obj-print-warn", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [300,230,200,22], text: "print [AC-WARN]" } },
    // ── Live track color integration ────────────────────────────────
    // live.thisdevice → canonical_parent resolves the track hosting
    // this device; live.observer watches its `color` property and re-
    // emits on change. Route the color int through sprintf into jweb
    // as window.acSetLiveTrackColor(N). Piece decodes 0xRRGGBB itself.
    { box: { id: "obj-live-thisdevice", maxclass: "newobj", numinlets: 1, numoutlets: 3, outlettype: ["bang","int","int"], patching_rect: [10,540,110,22], text: "live.thisdevice" } },
    { box: { id: "obj-live-path-parent", maxclass: "newobj", numinlets: 1, numoutlets: 3, outlettype: ["","",""], patching_rect: [10,570,280,22], text: "live.path this_device canonical_parent" } },
    { box: { id: "obj-live-observe-color", maxclass: "newobj", numinlets: 2, numoutlets: 3, outlettype: ["","",""], patching_rect: [10,600,220,22], text: "live.observer @property color" } },
    { box: { id: "obj-route-track-color", maxclass: "newobj", numinlets: 1, numoutlets: 2, outlettype: ["",""], patching_rect: [10,630,120,22], text: "route color" } },
    // Stash the track color int in an [i] so requestTrackColor can
    // bang it to re-emit post-boot (mirrors the focus pattern).
    { box: { id: "obj-track-color-i", maxclass: "newobj", numinlets: 2, numoutlets: 1, outlettype: ["int"], patching_rect: [10,655,40,22], text: "i" } },
    { box: { id: "obj-sprintf-track-color", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [10,680,480,22], text: "sprintf executejavascript window.acSetLiveTrackColor(%ld)" } },
    // ── Host-window focus detection ─────────────────────────────────
    // [active] fires 1 when Live's window becomes the foreground window
    // and 0 when it loses focus. Stashed in an [i] so requestFocus can
    // bang it to re-emit the current state (the initial fire can
    // happen before jweb navigates to the live URL, losing the push).
    { box: { id: "obj-active", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: ["int"], patching_rect: [10,700,60,22], text: "active" } },
    { box: { id: "obj-focus-i", maxclass: "newobj", numinlets: 2, numoutlets: 1, outlettype: ["int"], patching_rect: [10,725,40,22], text: "i" } },
    { box: { id: "obj-sprintf-focus", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [10,755,440,22], text: "sprintf executejavascript window.acSetLiveFocus(%ld)" } },
    // MIDI routing (downstream of the unmatched outlet).
    // `transport` carries spacebar-style play/pause requests from the
    // jweb iframe. `info` is a manual diagnostics dump bound to a
    // patcher button so the user can grab a state snapshot on demand.
    { box: { id: "obj-route", maxclass: "newobj", numinlets: 1, numoutlets: 10, outlettype: ["","","","","","","","","",""], patching_rect: [10,300,560,22], text: "route note channel notedown noteup octave focus ping transport info" } },
    { box: { id: "obj-noteout", maxclass: "newobj", numinlets: 2, numoutlets: 0, patching_rect: [10,450,60,22], text: "noteout" } },
    // MIDI thru: the device sits in the MIDI Effect chain ahead of the
    // instrument, so any MIDI arriving from a clip (or upstream device)
    // has to be forwarded explicitly — without these two boxes the
    // clip's notes hit the device, get consumed, and never reach the
    // instrument downstream. [noteout] above carries our jweb-driven
    // keypresses; this pair handles everything else (clip playback,
    // upstream MIDI effects, controller input on the track).
    { box: { id: "obj-midiin", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: ["int"], patching_rect: [400,420,60,22], text: "midiin" } },
    { box: { id: "obj-midiout", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [400,450,60,22], text: "midiout" } },
    { box: { id: "obj-pack-on", maxclass: "newobj", numinlets: 2, numoutlets: 1, outlettype: ["list"], patching_rect: [10,410,90,22], text: "pack 0 100" } },
    { box: { id: "obj-pack-off", maxclass: "newobj", numinlets: 2, numoutlets: 1, outlettype: ["list"], patching_rect: [120,410,90,22], text: "pack 0 0" } },
    { box: { id: "obj-sprintf-pong", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [10,340,320,22], text: "sprintf script window.acMaxPong(%ld)" } },
    // Live recording bridge — taps the same notedown/noteup/note stream
    // and writes notes into the recording clip via LiveAPI. Without this,
    // [noteout] plays the downstream instrument but Live records the
    // track INPUT, not the device's output, so nothing ends up in the clip.
    { box: { id: "obj-prepend-note-rec", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [10,790,90,22], text: "prepend note" } },
    { box: { id: "obj-prepend-down-rec", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [110,790,110,22], text: "prepend notedown" } },
    { box: { id: "obj-prepend-up-rec", maxclass: "newobj", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [230,790,100,22], text: "prepend noteup" } },
    // transport / info messages from jweb get prepended to a single
    // symbol so the v8.codebox dispatcher (function transport, function
    // info) picks them up. `info` button beside the device gives a
    // manual diagnostic dump without needing to type into Max console.
    { box: { id: "obj-msg-transport", maxclass: "message", numinlets: 2, numoutlets: 1, outlettype: [""], patching_rect: [340,790,80,22], text: "transport" } },
    { box: { id: "obj-msg-info", maxclass: "message", numinlets: 2, numoutlets: 1, outlettype: [""], patching_rect: [430,790,80,22], text: "info" } },
    // Print boxes so we can see whether the symbols ever land in Max —
    // if "TRANSPORT-IN" never appears in the Max console while you're
    // hitting spacebar, the iframe isn't receiving the keydown (Live's
    // host window has snatched it for native transport).
    { box: { id: "obj-print-transport", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [520,790,160,22], text: "print TRANSPORT-IN" } },
    { box: { id: "obj-recorder", maxclass: "v8.codebox", numinlets: 1, numoutlets: 1, outlettype: [""], patching_rect: [10,820,720,180], filename: "none", saved_object_attributes: { parameter_enable: 0 }, code: NOTEPAT_RECORDER_JS } },
  ];
  const lines = [
    { patchline: { source: ["obj-jweb", 2], destination: ["obj-route-top", 0] } },
    // Handshake "ready" fans out to every chunk message (see below).
    // `goonline` fires the `url …` message into jweb~, which navigates to
    // the live piece and abandons the bootstrap (chunks never fire).
    { patchline: { source: ["obj-route-top", 1], destination: ["obj-goonline-msg", 0] } },
    { patchline: { source: ["obj-goonline-msg", 0], destination: ["obj-jweb", 0] } },
    // `requestTrackColor` / `requestFocus` from piece: bang the stored
    // [i] so it re-emits the last value into jweb (the initial fires
    // can happen before the piece is mounted).
    { patchline: { source: ["obj-route-top", 2], destination: ["obj-track-color-i", 0] } },
    { patchline: { source: ["obj-route-top", 3], destination: ["obj-focus-i", 0] } },
    { patchline: { source: ["obj-route-top", 4], destination: ["obj-print-log", 0] } },
    { patchline: { source: ["obj-route-top", 5], destination: ["obj-print-error", 0] } },
    { patchline: { source: ["obj-route-top", 6], destination: ["obj-print-warn", 0] } },
    // Unmatched messages (notedown/noteup/octave/focus/ping) → MIDI router.
    { patchline: { source: ["obj-route-top", 7], destination: ["obj-route", 0] } },
    // Live track color chain — stashed in [i] so requestTrackColor can
    // bang it to re-emit:
    { patchline: { source: ["obj-live-thisdevice", 0], destination: ["obj-live-path-parent", 0] } },
    { patchline: { source: ["obj-live-path-parent", 0], destination: ["obj-live-observe-color", 0] } },
    { patchline: { source: ["obj-live-observe-color", 0], destination: ["obj-route-track-color", 0] } },
    { patchline: { source: ["obj-route-track-color", 0], destination: ["obj-track-color-i", 0] } },
    { patchline: { source: ["obj-track-color-i", 0], destination: ["obj-sprintf-track-color", 0] } },
    { patchline: { source: ["obj-sprintf-track-color", 0], destination: ["obj-jweb", 0] } },
    // Host-window focus chain: [active] → [i] (storage) → sprintf → jweb.
    // requestFocus bangs [i]'s left inlet to re-emit the stored value.
    { patchline: { source: ["obj-active", 0], destination: ["obj-focus-i", 0] } },
    { patchline: { source: ["obj-focus-i", 0], destination: ["obj-sprintf-focus", 0] } },
    { patchline: { source: ["obj-sprintf-focus", 0], destination: ["obj-jweb", 0] } },
    { patchline: { source: ["obj-route", 0], destination: ["obj-noteout", 0] } },
    { patchline: { source: ["obj-route", 1], destination: ["obj-noteout", 1] } },
    { patchline: { source: ["obj-route", 2], destination: ["obj-pack-on", 0] } },
    { patchline: { source: ["obj-route", 3], destination: ["obj-pack-off", 0] } },
    // Octave/focus: no MIDI wiring, just stream into the log prints
    // via route-top's log outlet already (skipped here — no-op on absence).
    { patchline: { source: ["obj-route", 6], destination: ["obj-sprintf-pong", 0] } },
    { patchline: { source: ["obj-sprintf-pong", 0], destination: ["obj-jweb", 0] } },
    { patchline: { source: ["obj-pack-on", 0], destination: ["obj-noteout", 0] } },
    { patchline: { source: ["obj-pack-off", 0], destination: ["obj-noteout", 0] } },
    // Forward any clip / upstream MIDI verbatim through the device.
    { patchline: { source: ["obj-midiin", 0], destination: ["obj-midiout", 0] } },
    // Tap note/notedown/noteup into the LiveAPI recorder.
    { patchline: { source: ["obj-route", 0], destination: ["obj-prepend-note-rec", 0] } },
    { patchline: { source: ["obj-route", 2], destination: ["obj-prepend-down-rec", 0] } },
    { patchline: { source: ["obj-route", 3], destination: ["obj-prepend-up-rec", 0] } },
    { patchline: { source: ["obj-prepend-note-rec", 0], destination: ["obj-recorder", 0] } },
    { patchline: { source: ["obj-prepend-down-rec", 0], destination: ["obj-recorder", 0] } },
    { patchline: { source: ["obj-prepend-up-rec", 0], destination: ["obj-recorder", 0] } },
    // transport (route outlet 7) → `transport` message → recorder.transport()
    // info (route outlet 8) and the manual button → `info` → recorder.info()
    { patchline: { source: ["obj-route", 7], destination: ["obj-msg-transport", 0] } },
    { patchline: { source: ["obj-route", 7], destination: ["obj-print-transport", 0] } },
    { patchline: { source: ["obj-route", 8], destination: ["obj-msg-info", 0] } },
    { patchline: { source: ["obj-msg-transport", 0], destination: ["obj-recorder", 0] } },
    { patchline: { source: ["obj-msg-info", 0], destination: ["obj-recorder", 0] } },
  ];
  // One message box per chunk. All share a single source (ready) so they
  // fire together — order doesn't matter, _ac.p reassembles by index.
  chunks.forEach((data, i) => {
    const id = "obj-chunk-" + i;
    const text = "executejavascript window._ac.p('" + i + "|" + chunks.length + "|" + data + "')";
    boxes.push({ box: { id, maxclass: "message", numinlets: 2, numoutlets: 1, outlettype: [""], patching_rect: [10, 480 + i * 4, 200, 22], text } });
    lines.push({ patchline: { source: ["obj-route-top", 0], destination: [id, 0] } });
    lines.push({ patchline: { source: [id, 0], destination: ["obj-jweb", 0] } });
  });
  return {
    patcher: {
      fileversion: 1,
      appversion: { major: 9, minor: 0, revision: 7, architecture: "x64", modernui: 1 },
      classnamespace: "box",
      rect: [100, 100, 900, 520],
      openrect: [0, 0, W, H],
      openinpresentation: 1,
      gridsize: [15, 15],
      enablehscroll: 0, enablevscroll: 0,
      devicewidth: W,
      description: `Aesthetic Computer ${pieceName} (offline, chunked) — ${chunks.length} chunks`,
      boxes, lines,
      dependency_cache: [], latency: 0, is_mpe: 0,
      external_mpe_tuning_enabled: 0, minimum_live_version: "",
      minimum_max_version: "", platform_compatibility: 0, autosave: 0,
    },
  };
}

// Pieces that need the bios.mjs DAW bridge (window.max.outlet for MIDI) and
// a piece-specific Max patcher + amxd kind. Auto-detected so callers don't
// need to pass flags — keeps the m4d CLI/prompt command one-liners.
// `profile` picks the patcher wiring; `kind` picks the amxd header tag
// (instrument=iiii vs midi_effect=mmmm) since Live uses it to categorize.
// `chunked: true` routes through the chunked bootstrap pipeline, required
// whenever the bundle exceeds Max's ~32 KB attribute/message text cap.
const DAW_PIECE_PROFILES = {
  "notepat-remote": { profile: "notepat", kind: "midi_effect", chunked: true, displayName: "notepat.com" },
};

export async function createM4DBundle(pieceName, isJSPiece, onProgress = () => {}, density = null) {
  onProgress({ stage: "fetch", message: `Building M4L device for ${pieceName}...` });

  const profileSpec = DAW_PIECE_PROFILES[pieceName] || { profile: "default", kind: "instrument", chunked: false };
  const { profile: pieceProfile, kind: amxdKind, chunked } = profileSpec;
  const forceDaw = pieceProfile !== "default";

  // Chunked pipeline always gzips the bundle (decompressed client-side by
  // the bootstrap). The single-URI pipeline can't use gzip inside jweb, so
  // it stays uncompressed when it's used.
  const bundleResult = isJSPiece
    ? await createJSPieceBundle(pieceName, onProgress, !chunked, density, false, false, false, forceDaw)
    : await createBundle(pieceName, onProgress, !chunked, density);

  let patcher;
  if (chunked) {
    onProgress({ stage: "generate", message: "Chunking bundle for M4L device..." });
    const { chunks, gzBytes, b64Bytes } = chunkBundleForM4D(bundleResult.html);
    onProgress({ stage: "generate", message: `Chunking: gzip=${Math.round(gzBytes/1024)}KB base64=${Math.round(b64Bytes/1024)}KB → ${chunks.length} chunks` });
    const bootstrapHTML = generateChunkedBootstrapHTML(pieceName);
    const bootstrapDataUri = `data:text/html;base64,${Buffer.from(bootstrapHTML).toString("base64")}`;
    if (bootstrapDataUri.length > 32000) {
      throw new Error(`chunked bootstrap data URI ${bootstrapDataUri.length} bytes exceeds 32000 cap`);
    }
    if (pieceProfile === "notepat") {
      patcher = generateChunkedNotepatM4DPatcher(pieceName, bootstrapDataUri, chunks);
    } else {
      throw new Error(`chunked pipeline has no patcher template for profile "${pieceProfile}"`);
    }
  } else {
    onProgress({ stage: "generate", message: "Embedding bundle in M4L device..." });
    const dataUri = `data:text/html;base64,${Buffer.from(bundleResult.html).toString("base64")}`;
    patcher = generateM4DPatcher(pieceName, dataUri, 400, 200, pieceProfile);
  }

  onProgress({ stage: "compress", message: "Packing .amxd binary..." });

  const binary = packAMXD(patcher, amxdKind);
  const displayName = profileSpec.displayName || pieceName;
  const filename = profileSpec.displayName
    ? `${displayName}.amxd`
    : `AC ${pieceName} (offline).amxd`;

  return { binary, filename, sizeKB: Math.round(binary.length / 1024) };
}

// ─── Self-extracting HTML wrapper ───────────────────────────────────

function renderBoxArt(title, boxArtPNG, inlineStyle = "") {
  if (!boxArtPNG) return "";
  const styleAttr = inlineStyle ? ` style="${inlineStyle}"` : "";
  // Render the image in normal DOM (visible to macOS Finder preview / og parsers),
  // then immediately hide it with a synchronous inline script — no flash.
  return `<img id="ac-box-art" src="data:image/png;base64,${boxArtPNG}" alt="${title}"${styleAttr}><script>document.getElementById('ac-box-art').style.display='none'<\/script>`;
}

function generateSelfExtractingHTML(title, gzipBase64, bgColor = null, boxArtPNG = null) {
  const bgRule = `background:${bgColor || "black"};`;
  const boxArtTag = renderBoxArt(
    title,
    boxArtPNG,
    "position:fixed;inset:0;width:100%;height:100%;object-fit:cover;pointer-events:none;"
  );
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${title} · Aesthetic Computer</title>
  <style>html,body{margin:0;padding:0;width:100%;height:100%;${bgRule}overflow:hidden}canvas{background:black}</style>
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
  const bgRule = `background:${bgColor || "black"};`;
  const boxArtTag = renderBoxArt(
    title,
    boxArtPNG,
    "position:fixed;inset:0;width:100%;height:100%;object-fit:cover;pointer-events:none;"
  );
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
  <style>html,body{margin:0;padding:0;width:100%;height:100%;${bgRule}overflow:hidden}canvas{background:black}</style>
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

function boxArtAccentColor(bgColor) {
  const palette = {
    red: "#ff6f61",
    green: "#46d89a",
    blue: "#56b4ff",
    yellow: "#ffd166",
    orange: "#ff9f43",
    cyan: "#44f6ff",
    black: "#a7b4c8",
    white: "#7b8798",
    gray: "#7fa0b5",
    grey: "#7fa0b5",
    brown: "#d19a66",
    navy: "#6aa2ff",
    teal: "#4fe5d2",
  };
  const key = String(bgColor || "").toLowerCase();
  if (key === "purple" || key === "magenta" || key === "pink" || key === "violet" || key === "indigo")
    return "#ffb454";
  return palette[key] || "#67e8f9";
}

function boxArtCodeColor(char, index) {
  if (char === "$") return "#ffd166";
  if (/\d/.test(char)) {
    const digitPalette = ["#ffadad", "#ffd6a5", "#fdffb6", "#caffbf", "#9bf6ff", "#a0c4ff"];
    return digitPalette[(Number(char) + index) % digitPalette.length];
  }
  const codePalette = ["#56e8ff", "#ffd166", "#ff8fab", "#8affc1", "#9fb6ff", "#ffb703"];
  return codePalette[index % codePalette.length];
}

function boxArtCodeTspans(display) {
  return Array.from(display).map((char, index) =>
    `<tspan fill="${boxArtCodeColor(char, index)}">${escapeXml(char)}</tspan>`
  ).join("");
}

async function generateBoxArtPNG(pieceName, authorHandle, bgColor, packDate) {
  const W = 800, H = 1000; // portrait 4:5
  const display = String(pieceName || "").trim();
  const len = Math.max(1, Array.from(display).length);
  const namePx = len <= 4 ? 190 : len <= 6 ? 162 : len <= 8 ? 140 : len <= 12 ? 118 : len <= 18 ? 94 : 76;
  const accent = boxArtAccentColor(bgColor);
  const codeTspans = boxArtCodeTspans(display);
  const nameY = H / 2;
  // Comic Relief Bold is installed as a system font on the oven server.
  // sharp/librsvg can only use system fonts, not @font-face data URIs.
  const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}">
  <defs>
    <linearGradient id="bg" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#07131f"/>
      <stop offset="55%" stop-color="#0f2532"/>
      <stop offset="100%" stop-color="#18313f"/>
    </linearGradient>
    <radialGradient id="glow-a" cx="18%" cy="20%" r="65%">
      <stop offset="0%" stop-color="${accent}" stop-opacity="0.34"/>
      <stop offset="100%" stop-color="${accent}" stop-opacity="0"/>
    </radialGradient>
    <radialGradient id="glow-b" cx="86%" cy="82%" r="72%">
      <stop offset="0%" stop-color="#ff9f43" stop-opacity="0.25"/>
      <stop offset="100%" stop-color="#ff9f43" stop-opacity="0"/>
    </radialGradient>
  </defs>
  <style>
    .kidlisp-comic { font-family: 'Comic Relief', 'Comic Sans MS', cursive; font-weight: 700; }
  </style>
  <rect width="${W}" height="${H}" fill="url(#bg)"/>
  <rect width="${W}" height="${H}" fill="url(#glow-a)"/>
  <rect width="${W}" height="${H}" fill="url(#glow-b)"/>
  <text x="${W / 2}" y="${nameY + 9}" class="kidlisp-comic"
    font-size="${namePx}" fill="rgba(0,0,0,0.45)"
    text-anchor="middle" dominant-baseline="middle">${escapeXml(display)}</text>
  <text x="${W / 2}" y="${nameY}" class="kidlisp-comic"
    font-size="${namePx}" text-anchor="middle" dominant-baseline="middle">${codeTspans}</text>
  ${packDate ? `<text x="${W / 2}" y="${H - 42}" class="kidlisp-comic"
    font-size="24" letter-spacing="2" fill="rgba(231,242,255,0.5)"
    text-anchor="middle" dominant-baseline="middle">${escapeXml(packDate)}</text>` : ""}
</svg>`;

  const buf = await sharp(Buffer.from(svg)).png({ compressionLevel: 9 }).toBuffer();
  return buf.toString("base64");
}

// ─── HTML templates ─────────────────────────────────────────────────
// These are large template literals — kept as-is from the Netlify version.

function generateHTMLBundle(opts) {
  const {
    PIECE_NAME, PIECE_NAME_NO_DOLLAR, mainSource, kidlispSources,
    files, paintingData, authorHandle, packDate, packTime, gitVersion, filename, density, bgColor, bdfGlyphs, boxArtPNG, keeplabel,
  } = opts;

  const bgRule = `background: ${bgColor || "black"}; `;
  const boxArtImg = renderBoxArt(PIECE_NAME, boxArtPNG);

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <title>${PIECE_NAME} · Aesthetic Computer</title>
  <style>
    html, body { margin: 0; padding: 0; width: 100%; height: 100%; ${bgRule}overflow: hidden; }
    canvas { display: block; image-rendering: pixelated; background: black; }
    #ac-box-art { position: fixed; inset: 0; width: 100%; height: 100%; object-fit: cover; object-position: center; pointer-events: none; }
  </style>
</head>
<body>
  ${boxArtImg}
  <script>
    // Phase 1: Setup VFS, blob URLs, import map, and fetch interception.
    // This MUST run in a regular <script> (not type="module") so the import map
    // is in the DOM BEFORE any <script type="module"> executes.
    window.acPACK_MODE = true;
    window.acUseWebGLComposite = false; // Disable WebGL composite in packed bundles to prevent black screen on IPFS/sandboxed contexts
    ${keeplabel ? `window.acKEEP_LABEL = true;` : ""}
    window.KIDLISP_SUPPRESS_SNAPSHOT_LOGS = true;
    window.__acKidlispConsoleEnabled = false;
    window.acKEEP_MODE = true;
    window.acSTARTING_PIECE = "${PIECE_NAME}";
    window.acPACK_PIECE = "${PIECE_NAME}";
    window.acPACK_DATE = "${packDate}";
    window.acPACK_GIT = "${gitVersion}";
    window.acKIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    ${density ? `window.acPACK_DENSITY = ${density};` : `// Smart density: match device.html logic for keep bundles
    (function() {
      var sw = window.screen.width * (window.devicePixelRatio || 1);
      var sh = window.screen.height * (window.devicePixelRatio || 1);
      var maxDim = Math.max(sw, sh);
      if (maxDim >= 3840) { window.acPACK_DENSITY = 8; }
      else if (Math.abs(sw - sh) < 100 && maxDim >= 1000 && maxDim <= 1200) { window.acPACK_DENSITY = 4; }
      else { window.acPACK_DENSITY = 3; }
    })();`}
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
      document.body.style.cssText='color:#fff;background:#400;padding:20px;font:12px monospace;white-space:pre-wrap';
      document.body.textContent='Boot failed: '+err.message+'\\n'+(err.stack||'');
      try { if (window.max && window.max.outlet) window.max.outlet('error', '[boot] ' + err.message + ' :: ' + (err.stack || '')); } catch(_){}
    }
  </script>
</body>
</html>`;
}

function generateJSPieceHTMLBundle(opts) {
  const { pieceName, files, packDate, packTime, gitVersion, bdfGlyphs, boxArtPNG, keeplabel, forceDaw } = opts;

  const boxArtImg = renderBoxArt(pieceName, boxArtPNG);

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <title>${pieceName} · Aesthetic Computer</title>
  <style>
    body { margin: 0; padding: 0; overflow: hidden; background: #222; }
    canvas { display: block; image-rendering: pixelated; }
    #ac-box-art { position: fixed; inset: 0; width: 100%; height: 100%; object-fit: cover; object-position: center; pointer-events: none; }
  </style>
</head>
<body>
  ${boxArtImg}
  ${forceDaw ? `<script>
    // Max console bridge — must run FIRST so we see anything that breaks
    // below. Forwards console.log/warn/error + window.onerror to
    // window.max.outlet, which [route log error warn] in the patcher
    // funnels into [print] for the Max Console. Without this a bundle
    // failure is invisible (gray jweb, no diagnostics).
    //
    // window.max may be injected slightly after page load — buffer early
    // messages and flush once the outlet shows up (or give up after 5s).
    (function(){
      var buf = [];
      var ready = false;
      function flush() {
        if (!window.max || typeof window.max.outlet !== 'function') return false;
        ready = true;
        for (var i = 0; i < buf.length; i++) {
          try { window.max.outlet(buf[i][0], buf[i][1]); } catch (_) {}
        }
        buf.length = 0;
        return true;
      }
      function emit(tag, args) {
        var parts = [];
        for (var i = 0; i < args.length; i++) {
          var a = args[i];
          if (a && a.stack) parts.push(String(a.stack));
          else if (typeof a === 'object') { try { parts.push(JSON.stringify(a)); } catch(_) { parts.push(String(a)); } }
          else parts.push(String(a));
        }
        var msg = parts.join(' ').slice(0, 900);
        if (ready) { try { window.max.outlet(tag, msg); } catch (_) {} }
        else buf.push([tag, msg]);
      }
      var orig = { log: console.log, warn: console.warn, error: console.error };
      console.log = function() { emit('log', arguments); orig.log.apply(console, arguments); };
      console.warn = function() { emit('warn', arguments); orig.warn.apply(console, arguments); };
      console.error = function() { emit('error', arguments); orig.error.apply(console, arguments); };
      window.addEventListener('error', function(e) {
        emit('error', ['[uncaught]', e.message, '@', (e.filename||'?') + ':' + (e.lineno||0) + ':' + (e.colno||0)]);
      });
      window.addEventListener('unhandledrejection', function(e) {
        emit('error', ['[unhandled-rejection]', (e.reason && e.reason.stack) || String(e.reason)]);
      });
      emit('log', ['[ac-bundle] jweb alive — piece=' + (${JSON.stringify(pieceName)}) + ' ua=' + navigator.userAgent]);
      // Flush as soon as window.max is injected.
      var tries = 0;
      var iv = setInterval(function() {
        if (flush() || ++tries > 50) clearInterval(iv);
      }, 100);
    })();
  </script>` : ""}
  <script>
    // Phase 1: Setup VFS, blob URLs, import map, and fetch interception.
    // This MUST run in a regular <script> (not type="module") so the import map
    // is in the DOM BEFORE any <script type="module"> executes.
    window.acPACK_MODE = true;
    ${keeplabel ? `window.acKEEP_LABEL = true;` : ""}
    ${forceDaw ? `window.acFORCE_DAW = true;
    // M4L devices always want density=1, nogap, and keep the corner HUD
    // label (matches the live URL's ?density=1&nogap params — the data:
    // URI has no query string, so we set the equivalent globals).
    window.acPACK_DENSITY = 1;
    window.acFORCE_NOGAP = true;
    window.acKEEP_LABEL = true;
    try { window.max && window.max.outlet && window.max.outlet("log", "[viewport] inner=" + innerWidth + "x" + innerHeight + " dpr=" + devicePixelRatio); } catch(_) {}` : ""}
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
