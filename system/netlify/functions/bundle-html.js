// bundle-html.js - Netlify Function [DEPRECATED — bundling now happens on oven]
// Generates self-contained HTML bundles on-demand via API
// Supports both KidLisp pieces ($code) and JavaScript pieces (piece=name)
//
// Usage:
//   GET /api/bundle-html?code=39j       - Bundle a KidLisp piece
//   GET /api/bundle-html?piece=notepat  - Bundle a JavaScript .mjs piece
//
// Returns: Self-extracting gzip-compressed HTML file
//
// Optimization: Core system files are cached per git commit to speed up
// subsequent bundle requests. Only piece-specific data is fetched per request.

const { promises: fs } = require("fs");
const fsSync = require("fs");
const path = require("path");
const { gzipSync } = require("zlib");
const https = require("https");
const { execSync } = require("child_process");

// Netlify streaming support
const { stream } = require("@netlify/functions");

// Get git commit from build-time env var, or dynamically from git in dev
function getGitCommit() {
  if (process.env.GIT_COMMIT) return process.env.GIT_COMMIT;
  try {
    const hash = execSync("git rev-parse --short HEAD", { encoding: "utf8" }).trim();
    const isDirty = execSync("git status --porcelain", { encoding: "utf8" }).trim().length > 0;
    return isDirty ? `${hash} (dirty)` : hash;
  } catch {
    return "unknown";
  }
}
const GIT_COMMIT = getGitCommit();
const CONTEXT = process.env.CONTEXT || "production";

// In-memory cache for core bundle (persists across warm function invocations)
// Key: git commit, Value: { coreFiles, fontFiles, timestamp }
let coreBundleCache = null;
let coreBundleCacheCommit = null;

// Custom fetch that ignores self-signed certs in dev mode
const devAgent = new https.Agent({ rejectUnauthorized: false });
async function devFetch(url, options = {}) {
  if (CONTEXT === 'dev' && url.startsWith('https://localhost')) {
    const { default: nodeFetch } = await import('node-fetch');
    return nodeFetch(url, { ...options, agent: devAgent });
  }
  return fetch(url, options);
}

// Essential files for KidLisp bundles (same as CLI)
const ESSENTIAL_FILES = [
  // Core system
  'boot.mjs',
  'bios.mjs',
  
  // Core loop and disk
  'lib/loop.mjs',
  'lib/disk.mjs',
  'lib/parse.mjs',
  
  // KidLisp interpreter
  'lib/kidlisp.mjs',
  
  // Graphics essentials
  'lib/graph.mjs',
  'lib/geo.mjs',
  'lib/2d.mjs',
  'lib/pen.mjs',
  'lib/num.mjs',
  'lib/gl.mjs',
  'lib/webgl-blit.mjs',
  
  // System essentials
  'lib/helpers.mjs',
  'lib/logs.mjs',
  'lib/store.mjs',
  'lib/platform.mjs',
  'lib/pack-mode.mjs',
  
  // BIOS dependencies
  'lib/keyboard.mjs',
  'lib/gamepad.mjs',
  'lib/motion.mjs',
  'lib/speech.mjs',
  'lib/help.mjs',
  'lib/midi.mjs',
  'lib/usb.mjs',
  'lib/headers.mjs',
  'lib/glaze.mjs',
  'lib/ui.mjs',
  
  // Disk dependencies
  'disks/common/tape-player.mjs',
  
  // Sound dependencies
  'lib/sound/sound-whitelist.mjs',
  
  // Audio worklet bundled for PACK mode
  'lib/speaker-bundled.mjs',
  
  // gl-matrix dependencies
  'dep/gl-matrix/common.mjs',
  'dep/gl-matrix/vec2.mjs',
  'dep/gl-matrix/vec3.mjs',
  'dep/gl-matrix/vec4.mjs',
  'dep/gl-matrix/mat3.mjs',
  'dep/gl-matrix/mat4.mjs',
  'dep/gl-matrix/quat.mjs',
  
  // Glaze dependencies
  'lib/glazes/uniforms.js',
];

const SKIP_FILES = [];

// Generate timestamp: YYYY.M.D.H.M.S.mmm
function timestamp(date = new Date()) {
  const pad = (n, digits = 2) => n.toString().padStart(digits, "0");
  return `${date.getFullYear()}.${date.getMonth() + 1}.${date.getDate()}.${date.getHours()}.${date.getMinutes()}.${date.getSeconds()}.${pad(date.getMilliseconds(), 3)}`;
}

// Extract painting short codes from KidLisp source
function extractPaintingCodes(source) {
  const codes = [];
  const regex = /#([a-zA-Z0-9]{3})\b/g;
  let match;
  while ((match = regex.exec(source)) !== null) {
    if (!codes.includes(match[1])) codes.push(match[1]);
  }
  return codes;
}

// Resolve painting code to handle+slug via API
async function resolvePaintingCode(code) {
  try {
    const baseUrl = CONTEXT === 'dev' ? 'https://localhost:8888' : 'https://aesthetic.computer';
    const response = await devFetch(`${baseUrl}/api/painting-code?code=${code}`);
    if (!response.ok) return null;
    const data = await response.json();
    return { code, handle: data.handle || 'anon', slug: data.slug };
  } catch {
    return null;
  }
}

// Fetch painting PNG as base64
async function fetchPaintingImage(handle, slug) {
  const handlePath = handle === 'anon' ? '' : `@${handle}/`;
  const url = `https://aesthetic.computer/media/${handlePath}painting/${slug}.png`;
  try {
    const response = await devFetch(url);
    if (!response.ok) return null;
    const buffer = await response.arrayBuffer();
    return Buffer.from(buffer).toString('base64');
  } catch {
    return null;
  }
}

// Fetch author info from user ID (AC handle and permanent user code)
async function fetchAuthorInfo(userId) {
  if (!userId) return { handle: null, userCode: null };
  
  let acHandle = null;
  let userCode = null;
  
  // Get AC handle via handle endpoint
  try {
    const baseUrl = CONTEXT === 'dev' ? 'https://localhost:8888' : 'https://aesthetic.computer';
    const response = await devFetch(`${baseUrl}/handle?for=${encodeURIComponent(userId)}`);
    if (response.ok) {
      const data = await response.json();
      if (data.handle) acHandle = data.handle; // Don't add @ prefix for filenames
    }
  } catch { /* ignore */ }
  
  // Get permanent user code from users collection
  try {
    const { connect } = await import('../../backend/database.mjs');
    const database = await connect();
    const users = database.db.collection('users');
    const user = await users.findOne({ _id: userId }, { projection: { code: 1 } });
    if (user?.code) {
      userCode = user.code;
    }
    await database.disconnect();
  } catch { /* ignore */ }
  
  return { handle: acHandle, userCode };
}

// Fetch KidLisp source from API
async function fetchKidLispFromAPI(pieceName) {
  const cleanName = pieceName.replace('$', '');
  const baseUrl = CONTEXT === 'dev' ? 'https://localhost:8888' : 'https://aesthetic.computer';
  const response = await devFetch(`${baseUrl}/api/store-kidlisp?code=${cleanName}`);
  const data = await response.json();
  
  if (data.error || !data.source) {
    throw new Error(`Piece '$${cleanName}' not found`);
  }
  
  return { source: data.source, userId: data.user || null };
}

// Extract KidLisp refs ($xxx)
function extractKidLispRefs(source) {
  const refs = [];
  const regex = /\$[a-z0-9_-]+/gi;
  for (const match of source.matchAll(regex)) {
    const ref = match[0].toLowerCase();
    if (!refs.includes(ref)) refs.push(ref);
  }
  return refs;
}

// Get KidLisp source with all dependencies
async function getKidLispSourceWithDeps(pieceName) {
  const allSources = {};
  const toProcess = [pieceName];
  const processed = new Set();
  let mainPieceUserId = null;
  
  while (toProcess.length > 0) {
    const current = toProcess.shift();
    const cleanName = current.replace('$', '');
    
    if (processed.has(cleanName)) continue;
    processed.add(cleanName);
    
    const { source, userId } = await fetchKidLispFromAPI(cleanName);
    allSources[cleanName] = source;
    
    if (cleanName === pieceName.replace('$', '') && userId) {
      mainPieceUserId = userId;
    }
    
    const refs = extractKidLispRefs(source);
    for (const ref of refs) {
      const refName = ref.replace('$', '');
      if (!processed.has(refName)) {
        toProcess.push(refName);
      }
    }
  }
  
  // Resolve author info (AC handle and permanent user code)
  let authorHandle = 'anon';
  let userCode = null;
  if (mainPieceUserId) {
    const authorInfo = await fetchAuthorInfo(mainPieceUserId);
    if (authorInfo.handle) authorHandle = authorInfo.handle;
    if (authorInfo.userCode) userCode = authorInfo.userCode;
  }
  
  return { sources: allSources, authorHandle, userCode };
}

// Resolve relative import path
function resolvePath(base, relative) {
  if (!relative.startsWith('.')) return relative;
  
  let dir = path.dirname(base);
  const parts = dir === '.' ? [] : dir.split('/').filter(p => p);
  const relParts = relative.split('/');
  
  for (const part of relParts) {
    if (part === '..') parts.pop();
    else if (part !== '.' && part !== '') parts.push(part);
  }
  
  return parts.join('/');
}

// Rewrite imports for VFS compatibility
function rewriteImports(code, filepath) {
  code = code.replace(/from\s*['"]aesthetic\.computer\/disks\/([^'"]+)['"]/g, 
    (match, p) => 'from \'ac/disks/' + p + '\'');
  
  code = code.replace(/import\s*\((['"]aesthetic\.computer\/disks\/([^'"]+)['")])\)/g, 
    (match, fullPath, p) => 'import(\'ac/disks/' + p + '\')');
  
  code = code.replace(/from\s*['"](\.\.\/[^'"]+|\.\/[^'"]+)(\?[^'"]+)?['"]/g, (match, p) => {
    const resolved = resolvePath(filepath, p);
    return 'from"' + resolved + '"';
  });
  
  code = code.replace(/import\s*\((['"](\.\.\/[^'"]+|\.\/[^'"]+)(\?[^'"]+)?['")])\)/g, (match, fullPath, p) => {
    const resolved = resolvePath(filepath, p);
    return 'import("' + resolved + '")';
  });
  
  // Handle template literal imports like import(`./lib/disk.mjs`)
  code = code.replace(/import\s*\(\`(\.\.\/[^\`]+|\.\/[^\`]+)\`\)/g, (match, p) => {
    const clean = p.split('?')[0];
    const resolved = resolvePath(filepath, clean);
    return 'import("' + resolved + '")';
  });
  
  // Also rewrite string literals that look like relative module paths (for wrapper functions like importWithRetry)
  // This catches patterns like: importWithRetry("./bios.mjs") or anyFunction("./lib/parse.mjs")
  // But be careful not to rewrite strings that aren't module paths
  code = code.replace(/\(\s*['"](\.\.?\/[^'"]+\.m?js)(\?[^'"]+)?['"]\s*\)/g, (match, p) => {
    const resolved = resolvePath(filepath, p);
    return '("' + resolved + '")';
  });
  
  return code;
}

// Global flag for skipping minification (set per-request)
let skipMinification = false;

// Minify JS content
async function minifyJS(content, relativePath) {
  const ext = path.extname(relativePath);
  if (ext !== ".mjs" && ext !== ".js") return content;
  
  let processedContent = rewriteImports(content, relativePath);
  
  // Skip minification if nominify flag is set
  if (skipMinification) {
    return processedContent;
  }
  
  try {
    const { minify } = require("terser");

    const result = await minify(processedContent, {
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
      format: { comments: false, ascii_only: false, ecma: 2020 }
    });

    return result.code || processedContent;
  } catch (err) {
    console.error(`[minifyJS] Failed to minify ${relativePath}:`, err.message);
    return processedContent;
  }
}

// Auto-discover dependencies from imports
async function discoverDependencies(acDir, essentialFiles, skipFiles) {
  const discovered = new Set(essentialFiles);
  const toProcess = [...essentialFiles];
  
  while (toProcess.length > 0) {
    const file = toProcess.shift();
    const fullPath = path.join(acDir, file);
    
    if (!fsSync.existsSync(fullPath)) continue;
    
    try {
      const content = await fs.readFile(fullPath, 'utf8');
      
      const importRegex = /from\s+["'](\.\.[^"']+|\.\/[^"']+)["']/g;
      const dynamicImportRegex = /import\s*\(\s*["'](\.\.[^"']+|\.\/[^"']+)["']\s*\)/g;
      
      let match;
      while ((match = importRegex.exec(content)) !== null) {
        const resolved = resolvePath(file, match[1]);
        if (skipFiles.some(skip => resolved.includes(skip))) continue;
        if (!discovered.has(resolved)) {
          discovered.add(resolved);
          toProcess.push(resolved);
        }
      }
      
      while ((match = dynamicImportRegex.exec(content)) !== null) {
        const resolved = resolvePath(file, match[1]);
        if (skipFiles.some(skip => resolved.includes(skip))) continue;
        if (!discovered.has(resolved)) {
          discovered.add(resolved);
          toProcess.push(resolved);
        }
      }
    } catch {
      // Ignore errors
    }
  }
  
  return Array.from(discovered);
}

// Build or retrieve cached core bundle (minified system files + fonts)
async function getCoreBundle(acDir, onProgress = () => {}, forceRefresh = false) {
  // Check if we have a valid cache for this git commit
  if (!forceRefresh && coreBundleCache && coreBundleCacheCommit === GIT_COMMIT) {
    console.log(`[bundle-html] Using cached core bundle for commit ${GIT_COMMIT}`);
    onProgress({ stage: 'cache-hit', message: 'Using cached core files...' });
    return coreBundleCache;
  }
  
  if (forceRefresh) {
    console.log(`[bundle-html] Force refresh - rebuilding core bundle...`);
  } else {
    console.log(`[bundle-html] Building core bundle for commit ${GIT_COMMIT}...`);
  }
  const coreFiles = {};
  
  onProgress({ stage: 'discover', message: 'Discovering dependencies...' });
  
  // Discover all dependencies
  const allFiles = await discoverDependencies(acDir, ESSENTIAL_FILES, SKIP_FILES);
  
  onProgress({ stage: 'minify', message: `Minifying ${allFiles.length} files...` });
  
  // Load and minify files in parallel batches for speed
  let minifiedCount = 0;
  const BATCH_SIZE = 10;
  for (let i = 0; i < allFiles.length; i += BATCH_SIZE) {
    const batch = allFiles.slice(i, i + BATCH_SIZE);
    const results = await Promise.all(batch.map(async (file) => {
      const fullPath = path.join(acDir, file);
      try {
        if (!fsSync.existsSync(fullPath)) return null;
        let content = await fs.readFile(fullPath, 'utf8');
        content = await minifyJS(content, file);
        return { file, content };
      } catch {
        return null;
      }
    }));
    for (const r of results) {
      if (r) {
        coreFiles[r.file] = { content: r.content, binary: false, type: path.extname(r.file).slice(1) };
        minifiedCount++;
      }
    }
    onProgress({ stage: 'minify', message: `Minified ${minifiedCount}/${allFiles.length} files...` });
  }
  
  // Load nanoid
  const nanoidPath = 'dep/nanoid/index.js';
  const nanoidFullPath = path.join(acDir, nanoidPath);
  if (fsSync.existsSync(nanoidFullPath)) {
    let content = await fs.readFile(nanoidFullPath, 'utf8');
    content = await minifyJS(content, nanoidPath);
    coreFiles[nanoidPath] = { content, binary: false, type: 'js' };
  }
  
  onProgress({ stage: 'fonts', message: 'Loading fonts...' });
  
  // Load font_1 glyphs
  const font1Dir = path.join(acDir, 'disks/drawings/font_1');
  const fontCategories = ['lowercase', 'uppercase', 'numbers', 'symbols'];
  
  for (const category of fontCategories) {
    const categoryDir = path.join(font1Dir, category);
    try {
      if (fsSync.existsSync(categoryDir)) {
        const glyphFiles = fsSync.readdirSync(categoryDir).filter(f => f.endsWith('.json'));
        for (const glyphFile of glyphFiles) {
          const glyphPath = path.join(categoryDir, glyphFile);
          const content = await fs.readFile(glyphPath, 'utf8');
          const vfsPath = `disks/drawings/font_1/${category}/${glyphFile}`;
          coreFiles[vfsPath] = { content, binary: false, type: 'json' };
        }
      }
    } catch {
      // Skip
    }
  }
  
  // Cache the result
  coreBundleCache = coreFiles;
  coreBundleCacheCommit = GIT_COMMIT;
  console.log(`[bundle-html] Cached core bundle: ${Object.keys(coreFiles).length} files`);
  
  return coreFiles;
}

// Create bundle for JavaScript .mjs pieces (notepat, metronome, etc.)
async function createJSPieceBundle(pieceName, onProgress = () => {}, nocompress = false, density = null) {
  onProgress({ stage: 'init', message: `Bundling ${pieceName}...` });
  
  const packTime = Date.now();
  const packDate = new Date().toLocaleString("en-US", {
    timeZone: "America/Los_Angeles",
    year: "numeric",
    month: "long",
    day: "numeric",
    hour: "numeric",
    minute: "2-digit",
    second: "2-digit",
    hour12: true,
  });
  
  const bundleTimestamp = timestamp();
  
  // Determine acDir
  const acDir = path.join(__dirname, "..", "..", "public", "aesthetic.computer");
  
  console.log("[bundle-html] JS piece bundle - acDir:", acDir);
  
  // Get core bundle (cached per git commit)
  const coreFiles = await getCoreBundle(acDir, onProgress);
  
  // Build VFS starting with core files
  const files = { ...coreFiles };
  
  // Check if the piece exists
  const piecePath = `disks/${pieceName}.mjs`;
  const pieceFullPath = path.join(acDir, piecePath);
  
  if (!fsSync.existsSync(pieceFullPath)) {
    throw new Error(`Piece '${pieceName}' not found at ${piecePath}`);
  }
  
  onProgress({ stage: 'piece', message: `Loading ${pieceName}.mjs...` });
  
  // Load the piece file - DO NOT minify the actual piece source code!
  // Only platform/system code gets minified. Piece code stays readable.
  const pieceContent = await fs.readFile(pieceFullPath, 'utf8');
  // Only rewrite imports, don't minify
  const rewrittenPiece = rewriteImports(pieceContent, piecePath);
  files[piecePath] = { content: rewrittenPiece, binary: false, type: 'mjs' };
  
  // Discover piece-specific dependencies
  const pieceDepFiles = await discoverDependencies(acDir, [piecePath], SKIP_FILES);
  
  onProgress({ stage: 'deps', message: `Found ${pieceDepFiles.length} dependencies...` });
  
  // Track which files are piece dependencies (in disks/ folder) vs platform code
  for (const depFile of pieceDepFiles) {
    if (files[depFile]) continue; // Already in core bundle
    
    const depFullPath = path.join(acDir, depFile);
    try {
      if (!fsSync.existsSync(depFullPath)) continue;
      let content = await fs.readFile(depFullPath, 'utf8');
      
      // Don't minify files in disks/ folder (piece code), only platform code
      const isPieceCode = depFile.startsWith('disks/');
      if (isPieceCode) {
        content = rewriteImports(content, depFile);
      } else {
        content = await minifyJS(content, depFile);
      }
      
      files[depFile] = { content, binary: false, type: path.extname(depFile).slice(1) };
    } catch {
      // Skip files that can't be loaded
    }
  }
  
  onProgress({ stage: 'generate', message: 'Generating HTML bundle...' });
  
  // Generate HTML bundle for JS piece
  const htmlContent = generateJSPieceHTMLBundle({
    pieceName,
    files,
    packDate,
    packTime,
    gitVersion: GIT_COMMIT,
  });
  
  const filename = `${pieceName}-${bundleTimestamp}.html`;
  
  onProgress({ stage: 'compress', message: nocompress ? 'Skipping compression (nocompress mode)...' : 'Compressing...' });
  
  // If nocompress is true, return the raw HTML without gzip wrapper
  // This is needed for devices without DecompressionStream support (e.g., FF1)
  if (nocompress) {
    return { html: htmlContent, filename, sizeKB: Math.round(htmlContent.length / 1024) };
  }
  
  // Create gzip-compressed self-extracting bundle (brotli not supported in DecompressionStream)
  const compressed = gzipSync(Buffer.from(htmlContent, 'utf-8'), { level: 9 });
  const base64 = compressed.toString('base64');

  const finalHtml = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${pieceName} · Aesthetic Computer</title>
  <style>body{margin:0;background:#000;overflow:hidden}</style>
</head>
<body>
  <script>
    // Use blob: URL instead of data: URL for CSP compatibility (objkt sandboxing)
    const b64='${base64}';
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
      .catch(e=>{document.body.style.color='#fff';document.body.textContent='Bundle error: '+e.message;});
  </script>
</body>
</html>`;

  return { html: finalHtml, filename, sizeKB: Math.round(finalHtml.length / 1024) };
}

// Generate HTML bundle for JavaScript pieces
function generateJSPieceHTMLBundle(opts) {
  const {
    pieceName,
    files,
    packDate,
    packTime,
    gitVersion,
  } = opts;

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <title>${pieceName} · Aesthetic Computer</title>
  <style>
    body { margin: 0; padding: 0; background: black; overflow: hidden; }
    canvas { display: block; image-rendering: pixelated; }
  </style>
</head>
<body>
  <script>
    // Phase 1: Setup VFS, blob URLs, import map, and fetch interception.
    // This MUST run in a regular <script> (not type="module") so the import map
    // is in the DOM BEFORE any <script type="module"> executes.
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
    window.VFS = ${JSON.stringify(files).replace(/<\/script>/g, '<\\/script>')};
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
        importMapEntries['aesthetic.computer/' + fp] = window.VFS_BLOB_URLS[fp];
        importMapEntries['/aesthetic.computer/' + fp] = window.VFS_BLOB_URLS[fp];
        importMapEntries['./aesthetic.computer/' + fp] = window.VFS_BLOB_URLS[fp];
        importMapEntries['https://aesthetic.computer/' + fp] = window.VFS_BLOB_URLS[fp];
        importMapEntries['./' + fp] = window.VFS_BLOB_URLS[fp];
        importMapEntries['../' + fp] = window.VFS_BLOB_URLS[fp];
        importMapEntries['../../' + fp] = window.VFS_BLOB_URLS[fp];
      }
    }
    var s = document.createElement('script');
    s.type = 'importmap';
    s.textContent = JSON.stringify({ imports: importMapEntries });
    document.head.appendChild(s);
    var originalFetch = window.fetch;
    window.fetch = function(url, options) {
      var urlStr = typeof url === 'string' ? url : url.toString();
      var vfsPath = decodeURIComponent(urlStr)
        .replace(/^https?:\\/\\/[^\\/]+\\//g, '')
        .replace(/^aesthetic\\.computer\\//g, '')
        .replace(/#.*$/g, '')
        .replace(/\\?.*$/g, '');
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
    // Phase 2: Boot the app. Import map is already in the DOM from Phase 1.
    import(window.VFS_BLOB_URLS['boot.mjs']).catch(err => {
      document.body.style.cssText='color:#fff;background:#000;padding:20px;font-family:monospace';
      document.body.textContent='Boot failed: '+err.message;
    });
  </script>
</body>
</html>`;
}

// M4L .amxd binary header for Instrument devices
// Format: "ampf" + 4-byte type marker + "meta" + 4 zero bytes + "ptch"
const M4L_HEADER_INSTRUMENT = Buffer.from(
  'ampf\x04\x00\x00\x00iiiimeta\x04\x00\x00\x00\x00\x00\x00\x00ptch', 'binary'
);

// Generate a Max for Live instrument patcher with an embedded offline HTML bundle
function generateM4DPatcher(pieceName, dataUri, width = 400, height = 200) {
  const density = 1.5;
  return {
    patcher: {
      fileversion: 1,
      appversion: { major: 9, minor: 0, revision: 7, architecture: "x64", modernui: 1 },
      classnamespace: "box",
      rect: [134.0, 174.0, 800.0, 600.0],
      openrect: [0.0, 0.0, width, height],
      openinpresentation: 1,
      gridsize: [15.0, 15.0],
      enablehscroll: 0,
      enablevscroll: 0,
      devicewidth: width,
      description: `Aesthetic Computer ${pieceName} (offline)`,
      boxes: [
        {
          box: {
            disablefind: 0,
            id: "obj-jweb",
            latency: 0,
            maxclass: "jweb~",
            numinlets: 1,
            numoutlets: 3,
            outlettype: ["signal", "signal", ""],
            patching_rect: [10.0, 50.0, width, height],
            presentation: 1,
            presentation_rect: [0.0, 0.0, width + 1, height + 1],
            rendermode: 1,
            url: dataUri
          }
        },
        {
          box: {
            id: "obj-plugout",
            maxclass: "newobj",
            numinlets: 2,
            numoutlets: 0,
            patching_rect: [10.0, 280.0, 75.0, 22.0],
            text: "plugout~ 1 2"
          }
        },
        {
          box: {
            id: "obj-thisdevice",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 3,
            outlettype: ["bang", "int", "int"],
            patching_rect: [350.0, 50.0, 85.0, 22.0],
            text: "live.thisdevice"
          }
        },
        {
          box: {
            id: "obj-print",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 0,
            patching_rect: [350.0, 80.0, 150.0, 22.0],
            text: `print [AC-${pieceName.toUpperCase()}]`
          }
        },
        {
          box: {
            id: "obj-route",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 2,
            outlettype: ["", ""],
            patching_rect: [350.0, 140.0, 60.0, 22.0],
            text: "route ready"
          }
        },
        {
          box: {
            id: "obj-activate",
            maxclass: "message",
            numinlets: 2,
            numoutlets: 1,
            outlettype: [""],
            patching_rect: [350.0, 170.0, 60.0, 22.0],
            text: "activate 1"
          }
        },
        {
          box: {
            id: "obj-jweb-print",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 0,
            patching_rect: [350.0, 110.0, 90.0, 22.0],
            text: "print [AC-JWEB]"
          }
        },
        {
          box: {
            id: "obj-route-logs",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 4,
            outlettype: ["", "", "", ""],
            patching_rect: [470.0, 140.0, 120.0, 22.0],
            text: "route log error warn"
          }
        },
        {
          box: {
            id: "obj-udpsend",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 0,
            patching_rect: [470.0, 210.0, 160.0, 22.0],
            text: "udpsend 127.0.0.1 7777"
          }
        },
        {
          box: {
            id: "obj-prepend-log",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 1,
            outlettype: [""],
            patching_rect: [470.0, 170.0, 55.0, 22.0],
            text: "prepend log"
          }
        },
        {
          box: {
            id: "obj-prepend-error",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 1,
            outlettype: [""],
            patching_rect: [530.0, 170.0, 65.0, 22.0],
            text: "prepend error"
          }
        },
        {
          box: {
            id: "obj-prepend-warn",
            maxclass: "newobj",
            numinlets: 1,
            numoutlets: 1,
            outlettype: [""],
            patching_rect: [600.0, 170.0, 60.0, 22.0],
            text: "prepend warn"
          }
        }
      ],
      lines: [
        { patchline: { destination: ["obj-plugout", 0], source: ["obj-jweb", 0] } },
        { patchline: { destination: ["obj-plugout", 1], source: ["obj-jweb", 1] } },
        { patchline: { destination: ["obj-print", 0], source: ["obj-thisdevice", 0] } },
        { patchline: { destination: ["obj-jweb-print", 0], source: ["obj-jweb", 2] } },
        { patchline: { destination: ["obj-route", 0], source: ["obj-jweb", 2] } },
        { patchline: { destination: ["obj-activate", 0], source: ["obj-route", 0] } },
        { patchline: { destination: ["obj-jweb", 0], source: ["obj-activate", 0] } },
        { patchline: { destination: ["obj-route-logs", 0], source: ["obj-jweb", 2] } },
        { patchline: { destination: ["obj-prepend-log", 0], source: ["obj-route-logs", 0] } },
        { patchline: { destination: ["obj-prepend-error", 0], source: ["obj-route-logs", 1] } },
        { patchline: { destination: ["obj-prepend-warn", 0], source: ["obj-route-logs", 2] } },
        { patchline: { destination: ["obj-udpsend", 0], source: ["obj-prepend-log", 0] } },
        { patchline: { destination: ["obj-udpsend", 0], source: ["obj-prepend-error", 0] } },
        { patchline: { destination: ["obj-udpsend", 0], source: ["obj-prepend-warn", 0] } }
      ],
      dependency_cache: [],
      latency: 0,
      is_mpe: 0,
      external_mpe_tuning_enabled: 0,
      minimum_live_version: "",
      minimum_max_version: "",
      platform_compatibility: 0,
      autosave: 0
    }
  };
}

// Build a complete .amxd binary from a patcher object
function packAMXD(patcher) {
  const patcherJson = Buffer.from(JSON.stringify(patcher));
  const lengthBuf = Buffer.alloc(4);
  lengthBuf.writeUInt32LE(patcherJson.length, 0);
  return Buffer.concat([M4L_HEADER_INSTRUMENT, lengthBuf, patcherJson]);
}

// Create an offline M4L device (.amxd) with an embedded HTML bundle
async function createM4DBundle(pieceName, isJSPiece, onProgress = () => {}, density = null) {
  onProgress({ stage: 'fetch', message: `Building M4L device for ${pieceName}...` });

  // Generate the offline HTML bundle using existing infrastructure
  const bundleResult = isJSPiece
    ? await createJSPieceBundle(pieceName, onProgress, false, density)
    : await createBundle(pieceName, onProgress, false, density);

  onProgress({ stage: 'generate', message: 'Embedding bundle in M4L device...' });

  // Encode the full HTML as a data: URI for jweb~
  const htmlBase64 = Buffer.from(bundleResult.html).toString('base64');
  const dataUri = `data:text/html;base64,${htmlBase64}`;

  // Generate the instrument patcher with the embedded bundle
  const patcher = generateM4DPatcher(pieceName, dataUri);

  onProgress({ stage: 'compress', message: 'Packing .amxd binary...' });

  // Pack into .amxd format
  const amxdBinary = packAMXD(patcher);
  const filename = `AC ${pieceName} (offline).amxd`;

  return { binary: amxdBinary, filename, sizeKB: Math.round(amxdBinary.length / 1024) };
}

// Main bundle creation for KidLisp pieces
async function createBundle(pieceName, onProgress = () => {}, nocompress = false, density = null) {
  const PIECE_NAME_NO_DOLLAR = pieceName.replace(/^\$/, '');
  const PIECE_NAME = '$' + PIECE_NAME_NO_DOLLAR;
  
  onProgress({ stage: 'fetch', message: `Fetching $${PIECE_NAME_NO_DOLLAR}...` });
  
  // Fetch KidLisp source with dependencies
  const { sources: kidlispSources, authorHandle, userCode } = await getKidLispSourceWithDeps(PIECE_NAME_NO_DOLLAR);
  const mainSource = kidlispSources[PIECE_NAME_NO_DOLLAR];
  const depCount = Object.keys(kidlispSources).length - 1;
  
  onProgress({ stage: 'deps', message: `Found ${depCount} dependenc${depCount === 1 ? 'y' : 'ies'}` });
  
  const packTime = Date.now();
  const packDate = new Date().toLocaleString("en-US", {
    timeZone: "America/Los_Angeles",
    year: "numeric",
    month: "long",
    day: "numeric",
    hour: "numeric",
    minute: "2-digit",
    second: "2-digit",
    hour12: true,
  });
  
  const bundleTimestamp = timestamp();
  
  // Determine acDir - in Netlify function context, use __dirname to find bundled files
  const acDir = path.join(__dirname, "..", "..", "public", "aesthetic.computer");
  
  console.log("[bundle-html] acDir:", acDir);
  console.log("[bundle-html] acDir exists:", fsSync.existsSync(acDir));
  
  // Get core bundle (cached per git commit)
  const coreFiles = await getCoreBundle(acDir, onProgress);
  
  // Build VFS starting with core files
  const files = { ...coreFiles };
  
  // Extract and embed painting images (per-piece)
  const allKidlispSource = Object.values(kidlispSources).join('\n');
  const paintingCodes = extractPaintingCodes(allKidlispSource);
  const paintingData = {};
  
  if (paintingCodes.length > 0) {
    onProgress({ stage: 'paintings', message: `Embedding ${paintingCodes.length} painting${paintingCodes.length === 1 ? '' : 's'}...` });
  }
  
  for (const code of paintingCodes) {
    const resolved = await resolvePaintingCode(code);
    if (resolved) {
      paintingData[code] = resolved;
      const imageBase64 = await fetchPaintingImage(resolved.handle, resolved.slug);
      if (imageBase64) {
        const vfsPath = `paintings/${code}.png`;
        files[vfsPath] = { content: imageBase64, binary: true, type: 'png' };
      }
    }
  }
  
  // Create synthetic .lisp files (per-piece)
  for (const [name, source] of Object.entries(kidlispSources)) {
    const pieceLispPath = `disks/${name}.lisp`;
    files[pieceLispPath] = { content: source, binary: false, type: 'lisp' };
  }
  
  onProgress({ stage: 'generate', message: 'Generating HTML bundle...' });
  
  // Generate filename first so it can be included in colophon
  const filename = `$${PIECE_NAME_NO_DOLLAR}-${authorHandle}-${bundleTimestamp}.lisp.html`;
  
  // Generate HTML bundle (same template as CLI)
  const htmlContent = generateHTMLBundle({
    PIECE_NAME,
    PIECE_NAME_NO_DOLLAR,
    mainSource,
    kidlispSources,
    files,
    paintingData,
    authorHandle,
    packDate,
    packTime,
    gitVersion: GIT_COMMIT,
    filename,
    density, // Pass density for FF1/device performance
  });
  
  onProgress({ stage: 'compress', message: nocompress ? 'Skipping compression (nocompress mode)...' : 'Compressing...' });
  
  // If nocompress is true, return the raw HTML without gzip wrapper
  // This is needed for devices without DecompressionStream support (e.g., FF1)
  if (nocompress) {
    return { 
      html: htmlContent, 
      filename, 
      sizeKB: Math.round(htmlContent.length / 1024),
      mainSource,
      authorHandle,
      userCode,
      packDate,
      depCount,
    };
  }
  
  // Create gzip-compressed self-extracting bundle (brotli not supported in DecompressionStream)
  const compressed = gzipSync(Buffer.from(htmlContent, 'utf-8'), { level: 9 });
  const base64 = compressed.toString('base64');

  const finalHtml = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${PIECE_NAME} · Aesthetic Computer</title>
  <style>body{margin:0;background:#000;overflow:hidden}</style>
</head>
<body>
  <script>
    // Use blob: URL instead of data: URL for CSP compatibility (objkt sandboxing)
    const b64='${base64}';
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
      .catch(e=>{document.body.style.color='#fff';document.body.textContent='Bundle error: '+e.message;});
  </script>
</body>
</html>`;

  return {
    html: finalHtml,
    filename,
    sizeKB: Math.round(finalHtml.length / 1024),
    mainSource,
    authorHandle,
    userCode,
    packDate,
    depCount,
  };
}

// Generate the inner HTML bundle
function generateHTMLBundle(opts) {
  const {
    PIECE_NAME,
    PIECE_NAME_NO_DOLLAR,
    mainSource,
    kidlispSources,
    files,
    paintingData,
    authorHandle,
    packDate,
    packTime,
    gitVersion,
    filename,
    density,
  } = opts;

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <title>${PIECE_NAME} · Aesthetic Computer</title>
  <style>
    body { margin: 0; padding: 0; background: black; overflow: hidden; }
    canvas { display: block; image-rendering: pixelated; }
  </style>
</head>
<body>
  <script type="module">
    window.acPACK_MODE = true; // Required for import map resolution from blob URLs
    window.KIDLISP_SUPPRESS_SNAPSHOT_LOGS = true; // Disable console screenshots
    window.__acKidlispConsoleEnabled = false; // Disable KidLisp console auto-snaps
    window.acKEEP_MODE = true;
    window.acSTARTING_PIECE = "${PIECE_NAME}";
    window.acPACK_PIECE = "${PIECE_NAME}";
    window.acPACK_DATE = "${packDate}";
    window.acPACK_GIT = "${gitVersion}";
    window.acKIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    
    // Set density if provided (e.g., density=8 for FF1/device mode)
    ${density ? `window.acPACK_DENSITY = ${density};` : '// No density override - using default'}
    
    window.acPACK_COLOPHON = {
      piece: {
        name: '${PIECE_NAME_NO_DOLLAR}',
        sourceCode: ${JSON.stringify(mainSource)},
        isKidLisp: true
      },
      build: {
        author: '${authorHandle}',
        packTime: ${packTime},
        gitCommit: '${gitVersion}',
        gitIsDirty: false,
        fileCount: ${Object.keys(files).length},
        filename: '${filename}'
      }
    };
    
    window.acPAINTING_CODE_MAP = ${JSON.stringify(paintingData)};
    window.VFS = ${JSON.stringify(files).replace(/<\/script>/g, '<\\/script>')};
    
    window.acEMBEDDED_PAINTING_BITMAPS = {};
    window.acPAINTING_BITMAPS_READY = false;
    
    async function decodePaintingToBitmap(code, base64Data) {
      return new Promise((resolve, reject) => {
        const img = new Image();
        img.onload = function() {
          const canvas = document.createElement('canvas');
          canvas.width = img.width;
          canvas.height = img.height;
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
      const paintingPromises = [];
      for (const [code, info] of Object.entries(window.acPAINTING_CODE_MAP || {})) {
        const vfsPath = 'paintings/' + code + '.png';
        if (window.VFS && window.VFS[vfsPath]) {
          const promise = decodePaintingToBitmap(code, window.VFS[vfsPath].content)
            .then(bitmap => {
              window.acEMBEDDED_PAINTING_BITMAPS['#' + code] = bitmap;
              window.acEMBEDDED_PAINTING_BITMAPS[code] = bitmap;
            })
            .catch(() => {});
          paintingPromises.push(promise);
        }
      }
      await Promise.all(paintingPromises);
      window.acPAINTING_BITMAPS_READY = true;
    })();
    
    window.EMBEDDED_KIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    window.EMBEDDED_KIDLISP_PIECE = '${PIECE_NAME_NO_DOLLAR}';
    window.objktKidlispCodes = ${JSON.stringify(kidlispSources)};
    window.acPREFILL_CODE_CACHE = ${JSON.stringify(kidlispSources)};
    
    const originalAppendChild = Element.prototype.appendChild;
    Element.prototype.appendChild = function(child) {
      if (child.tagName === 'LINK' && child.rel === 'stylesheet' && child.href && child.href.includes('.css')) {
        return child;
      }
      return originalAppendChild.call(this, child);
    };
    
    // Also intercept HTMLBodyElement.append for font CSS loading
    const originalBodyAppend = HTMLBodyElement.prototype.append;
    HTMLBodyElement.prototype.append = function(...nodes) {
      const filteredNodes = nodes.filter(node => {
        if (node.tagName === 'LINK' && node.rel === 'stylesheet') {
          return false;
        }
        return true;
      });
      return originalBodyAppend.call(this, ...filteredNodes);
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
    for (const filepath of window.modulePaths) {
      if (window.VFS_BLOB_URLS[filepath]) {
        // Only add essential mappings to reduce import map size
        importMapEntries[filepath] = window.VFS_BLOB_URLS[filepath];
        importMapEntries['/' + filepath] = window.VFS_BLOB_URLS[filepath];
        importMapEntries[\`./\${filepath}\`] = window.VFS_BLOB_URLS[filepath];
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
        const codeMatch = urlStr.match(/[?&]code=([^&]+)/);
        if (codeMatch) {
          const code = codeMatch[1];
          const paintingInfo = window.acPAINTING_CODE_MAP[code];
          if (paintingInfo) {
            return Promise.resolve(new Response(JSON.stringify({
              code: paintingInfo.code,
              handle: paintingInfo.handle,
              slug: paintingInfo.slug
            }), { status: 200, headers: { 'Content-Type': 'application/json' } }));
          }
        }
        return Promise.resolve(new Response(JSON.stringify({ error: 'Not found' }), { status: 404 }));
      }
      
      let vfsPath = decodeURIComponent(urlStr)
        .replace(/^https?:\\/\\/[^\\/]+\\//g, '')
        .replace(/^aesthetic\\.computer\\//g, '')
        .replace(/#.*$/g, '')
        .replace(/\\?.*$/g, '');
      
      vfsPath = vfsPath.replace(/^\\.\\.\\/+/g, '').replace(/^\\.\\//g, '').replace(/^\\//g, '').replace(/^aesthetic\\.computer\\//g, '');
      
      if (urlStr.includes('/media/') && urlStr.includes('/painting/')) {
        for (const [code, info] of Object.entries(window.acPAINTING_CODE_MAP || {})) {
          if (urlStr.includes(info.slug)) {
            const paintingVfsPath = 'paintings/' + code + '.png';
            if (window.VFS[paintingVfsPath]) {
              const file = window.VFS[paintingVfsPath];
              const binaryStr = atob(file.content);
              const bytes = new Uint8Array(binaryStr.length);
              for (let i = 0; i < binaryStr.length; i++) {
                bytes[i] = binaryStr.charCodeAt(i);
              }
              return Promise.resolve(new Response(bytes, {
                status: 200,
                headers: { 'Content-Type': 'image/png' }
              }));
            }
          }
        }
      }
      
      if (window.VFS[vfsPath]) {
        const file = window.VFS[vfsPath];
        let content;
        let contentType = 'text/plain';
        
        if (file.binary) {
          const binaryStr = atob(file.content);
          const bytes = new Uint8Array(binaryStr.length);
          for (let i = 0; i < binaryStr.length; i++) {
            bytes[i] = binaryStr.charCodeAt(i);
          }
          content = bytes;
          if (file.type === 'png') contentType = 'image/png';
          else if (file.type === 'jpg' || file.type === 'jpeg') contentType = 'image/jpeg';
        } else {
          content = file.content;
          if (file.type === 'mjs' || file.type === 'js') contentType = 'application/javascript';
          else if (file.type === 'json') contentType = 'application/json';
        }
        
        return Promise.resolve(new Response(content, {
          status: 200,
          headers: { 'Content-Type': contentType }
        }));
      }
      
      // Silently handle expected missing files (fonts, .mjs pieces, cursors, SVGs, and CSS)
      if (vfsPath.includes('disks/drawings/font_') || vfsPath.endsWith('.mjs') || vfsPath.includes('cursors/') || vfsPath.endsWith('.svg') || vfsPath.endsWith('.css') || urlStr.includes('/type/webfonts/')) {
        return Promise.resolve(new Response('', { status: 200, headers: { 'Content-Type': 'text/css' } }));
      }
      
      return originalFetch.call(this, url, options);
    };
    
    // 📊 Bundle Telemetry - collect FPS and boot data
    (function initTelemetry() {
      // Skip if not on aesthetic.computer domain (sandboxed iframes have null origin)
      const origin = window.location?.origin || '';
      const isAcDomain = origin.includes('aesthetic.computer') || origin.includes('localhost');
      // Telemetry origin check (silent)
      if (!isAcDomain) return;
      
      const bootStart = performance.now();
      const sessionId = Math.random().toString(36).slice(2) + Date.now().toString(36);
      const telemetryUrl = 'https://aesthetic.computer/api/bundle-telemetry';
      
      // Collect performance samples
      const perfSamples = [];
      let sampleCount = 0;
      const maxSamples = 60; // 1 minute of data at 1 sample/second
      
      function sendTelemetry(type, data) {
        try {
          const payload = {
            type,
            data: {
              sessionId,
              piece: window.acPACK_PIECE || 'unknown',
              density: window.acPACK_DENSITY || 2,
              screenWidth: window.innerWidth,
              screenHeight: window.innerHeight,
              devicePixelRatio: window.devicePixelRatio || 1,
              userAgent: navigator.userAgent,
              ...data
            }
          };
          const body = JSON.stringify(payload);
          // Try sendBeacon first, fall back to fetch
          if (navigator.sendBeacon && navigator.sendBeacon(telemetryUrl, body)) {
            return;
          }
          fetch(telemetryUrl, { method: 'POST', body, headers: {'Content-Type': 'application/json'}, keepalive: true }).catch(() => {});
        } catch (e) { console.warn('Telemetry error:', e); }
      }
      
      // Send boot telemetry once loaded
      window.addEventListener('load', () => {
        const bootTime = performance.now() - bootStart;
        sendTelemetry('boot', {
          bootTime: Math.round(bootTime),
          vfsFileCount: Object.keys(window.VFS || {}).length,
          blobUrlCount: Object.keys(window.VFS_BLOB_URLS || {}).length,
        });
      });
      
      // Collect FPS samples every second
      let lastFrameTime = performance.now();
      let frameCount = 0;
      
      function measureFrame() {
        frameCount++;
        const now = performance.now();
        if (now - lastFrameTime >= 1000) {
          const fps = frameCount;
          frameCount = 0;
          lastFrameTime = now;
          
          if (sampleCount < maxSamples) {
            perfSamples.push({ t: Math.round(now - bootStart), fps });
            sampleCount++;
            
            // Send perf batch every 10 samples
            if (sampleCount % 10 === 0) {
              sendTelemetry('perf', { samples: perfSamples.slice(-10) });
            }
          }
        }
        requestAnimationFrame(measureFrame);
      }
      requestAnimationFrame(measureFrame);
      
      // Send error telemetry
      window.addEventListener('error', (e) => {
        sendTelemetry('error', {
          message: e.message,
          filename: e.filename,
          lineno: e.lineno,
          colno: e.colno,
        });
      });
    })();

    (async function() {
      if (window.acDecodePaintingsPromise) {
        await window.acDecodePaintingsPromise;
      }
      import(window.VFS_BLOB_URLS['boot.mjs']).catch(err => {
        document.body.style.color='#fff';
        document.body.textContent='Boot failed: '+err.message;
      });
    })();
  </script>
</body>
</html>`;
}

// Main handler - uses Netlify streaming adapter
exports.handler = stream(async (event) => {
  const code = event.queryStringParameters?.code;
  const piece = event.queryStringParameters?.piece;
  const format = event.queryStringParameters?.format || 'html';
  const nocache = event.queryStringParameters?.nocache === '1' || event.queryStringParameters?.nocache === 'true';
  const nocompress = event.queryStringParameters?.nocompress === '1' || event.queryStringParameters?.nocompress === 'true';
  const nominify = event.queryStringParameters?.nominify === '1' || event.queryStringParameters?.nominify === 'true';
  const inline = event.queryStringParameters?.inline === '1' || event.queryStringParameters?.inline === 'true';
  const density = parseInt(event.queryStringParameters?.density) || null; // e.g., density=8 for FF1
  const mode = event.queryStringParameters?.mode; // 'device' for simple iframe wrapper
  
  // Device mode: return a simple iframe wrapper (like device.kidlisp.com)
  // This is much faster and more reliable on devices like FF1
  if (mode === 'device') {
    const pieceCode = code || piece;
    if (!pieceCode) {
      return {
        statusCode: 400,
        headers: { "Content-Type": "text/plain" },
        body: "Missing code or piece parameter",
      };
    }
    
    const densityParam = density ? `?density=${density}` : '';
    const pieceUrl = `https://aesthetic.computer/${pieceCode}${densityParam}`;
    
    const deviceHtml = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <title>${pieceCode} · Aesthetic Computer (Device)</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    html, body { width: 100%; height: 100%; overflow: hidden; background: black; }
    iframe { width: 100%; height: 100%; border: none; }
  </style>
</head>
<body>
  <iframe src="${pieceUrl}" allow="autoplay; fullscreen"></iframe>
</body>
</html>`;
    
    return {
      statusCode: 200,
      headers: {
        "Content-Type": "text/html; charset=utf-8",
        "Cache-Control": "public, max-age=60",
      },
      body: deviceHtml,
    };
  }
  
  // Set minification flag (nominify=1 skips SWC minification for debugging)
  skipMinification = nominify;
  
  // Force cache refresh if requested (also needed when nominify changes)
  if (nocache || nominify) {
    coreBundleCache = null;
    coreBundleCacheCommit = null;
  }
  
  // Determine which type of bundle to create
  const isJSPiece = !!piece;
  const bundleTarget = piece || code;
  
  if (!bundleTarget) {
    return {
      statusCode: 400,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ 
        error: "Missing 'code' or 'piece' parameter.",
        usage: {
          kidlisp: "/api/bundle-html?code=39j",
          javascript: "/api/bundle-html?piece=notepat"
        }
      }),
    };
  }
  
  // M4D mode: generate an offline .amxd Max for Live device
  if (format === 'm4d') {
    try {
      const onProgress = (progress) => {
        console.log(`[bundle-html] m4d ${progress.stage}: ${progress.message}`);
      };

      const { binary, filename, sizeKB } = await createM4DBundle(
        bundleTarget, isJSPiece, onProgress, density
      );

      return {
        statusCode: 200,
        headers: {
          "Content-Type": "application/octet-stream",
          "Content-Disposition": `attachment; filename="${filename}"`,
          "Content-Length": binary.length.toString(),
          "Cache-Control": "no-cache",
        },
        body: binary.toString('base64'),
        isBase64Encoded: true,
      };
    } catch (error) {
      console.error("M4D bundle creation failed:", error);
      return {
        statusCode: 500,
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ error: error.message }),
      };
    }
  }

  // Streaming mode with SSE progress updates
  if (format === 'stream') {
    const readable = new ReadableStream({
      async start(controller) {
        const encoder = new TextEncoder();
        
        const sendEvent = (eventType, data) => {
          controller.enqueue(encoder.encode(`event: ${eventType}\ndata: ${JSON.stringify(data)}\n\n`));
        };
        
        try {
          const onProgress = (progress) => {
            sendEvent('progress', progress);
          };
          
          const { html, filename, sizeKB } = isJSPiece
            ? await createJSPieceBundle(bundleTarget, onProgress, nocompress, density)
            : await createBundle(bundleTarget, onProgress, nocompress, density);
          
          sendEvent('complete', {
            filename,
            content: Buffer.from(html).toString('base64'),
            sizeKB,
          });
          
          controller.close();
        } catch (error) {
          console.error("Bundle creation failed:", error);
          sendEvent('error', { error: error.message });
          controller.close();
        }
      }
    });
    
    return {
      statusCode: 200,
      headers: {
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        "Connection": "keep-alive",
      },
      body: readable,
    };
  }
  
  // Non-streaming modes
  try {
    const progressLog = [];
    const onProgress = (progress) => {
      progressLog.push(progress.message);
      console.log(`[bundle-html] ${progress.stage}: ${progress.message}`);
    };
    
    const bundleResult = isJSPiece
      ? await createJSPieceBundle(bundleTarget, onProgress, nocompress, density)
      : await createBundle(bundleTarget, onProgress, nocompress, density);
    
    const { html, filename, sizeKB, mainSource, authorHandle, userCode, packDate, depCount } = bundleResult;
    
    if (format === 'json' || format === 'base64') {
      return {
        statusCode: 200,
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          filename,
          content: Buffer.from(html).toString('base64'),
          sizeKB,
          progress: progressLog,
          // KidLisp-specific metadata (undefined for JS pieces)
          sourceCode: mainSource,
          authorHandle,
          userCode,
          packDate,
          depCount,
        }),
      };
    }
    
    // Default: return as HTML (inline for viewing, attachment for download)
    const headers = {
      "Content-Type": "text/html; charset=utf-8",
      "Cache-Control": "public, max-age=3600",
    };
    
    // Only add Content-Disposition: attachment if NOT inline mode
    // inline=1 serves the HTML directly for viewing in browser/FF1
    if (!inline) {
      headers["Content-Disposition"] = `attachment; filename="${filename}"`;
    }
    
    return {
      statusCode: 200,
      headers,
      body: html,
    };
    
  } catch (error) {
    console.error("Bundle creation failed:", error);
    return {
      statusCode: 500,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ error: error.message }),
    };
  }
});