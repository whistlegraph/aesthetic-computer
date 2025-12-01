// bundle-html.js - Netlify Function
// Generates KidLisp .lisp.html bundles on-demand via API
// Ported from tezos/bundle-keep-html.mjs CLI tool
//
// Usage: GET /api/bundle-html?code=39j
// Returns: Self-extracting gzip-compressed .lisp.html file
//
// Optimization: Core system files are cached per git commit to speed up
// subsequent bundle requests. Only piece-specific data (KidLisp source,
// paintings) are fetched per request.

const { promises: fs } = require("fs");
const fsSync = require("fs");
const path = require("path");
const { gzipSync } = require("zlib");
const https = require("https");

// Netlify streaming support
const { stream } = require("@netlify/functions");

// Get git commit from build-time env var or fallback
const GIT_COMMIT = process.env.GIT_COMMIT || "unknown";
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

// Fetch author handle from user ID
async function fetchAuthorHandle(userId) {
  if (!userId) return null;
  try {
    const baseUrl = CONTEXT === 'dev' ? 'https://localhost:8888' : 'https://aesthetic.computer';
    const response = await devFetch(`${baseUrl}/handle?for=${encodeURIComponent(userId)}`);
    if (!response.ok) return null;
    const data = await response.json();
    return data.handle ? `@${data.handle}` : null;
  } catch {
    return null;
  }
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
  
  // Resolve author handle
  let authorHandle = '@anon';
  if (mainPieceUserId) {
    const handle = await fetchAuthorHandle(mainPieceUserId);
    if (handle) authorHandle = handle;
  }
  
  return { sources: allSources, authorHandle };
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
  
  code = code.replace(/from\s*['"](\.\.\/[^'"]+|\.\/[^'"]+)['"]/g, (match, p) => {
    const resolved = resolvePath(filepath, p);
    return 'from"' + resolved + '"';
  });
  
  code = code.replace(/import\s*\((['"](\.\.\/[^'"]+|\.\/[^'"]+)['")])\)/g, (match, fullPath, p) => {
    const resolved = resolvePath(filepath, p);
    return 'import("' + resolved + '")';
  });
  
  // Handle template literal imports like import(`./lib/disk.mjs`)
  code = code.replace(/import\s*\(\`(\.\.\/[^\`]+|\.\/[^\`]+)\`\)/g, (match, p) => {
    const resolved = resolvePath(filepath, p);
    return 'import("' + resolved + '")';
  });
  
  return code;
}

// Minify JS content
async function minifyJS(content, relativePath) {
  const ext = path.extname(relativePath);
  if (ext !== ".mjs" && ext !== ".js") return content;
  
  let processedContent = rewriteImports(content, relativePath);
  
  try {
    const { minify } = require("@swc/wasm");
    const result = await minify(processedContent, {
      compress: {
        dead_code: true,
        unused: true,
        passes: 3,
        pure_getters: true,
        unsafe_math: true,
        join_vars: true,
        sequences: true,
        evaluate: true,
        conditionals: true,
        booleans: true,
        loops: true,
        side_effects: true,
        collapse_vars: true,
        reduce_vars: true,
        inline: 3
      },
      mangle: {
        toplevel: true,
        keep_classnames: false,
        keep_fnames: false,
        safari10: false
      },
      format: { comments: false, ascii_only: false },
      module: true,
      sourceMap: false
    });
    
    return result.code || processedContent;
  } catch {
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
async function getCoreBundle(acDir, onProgress = () => {}) {
  // Check if we have a valid cache for this git commit
  if (coreBundleCache && coreBundleCacheCommit === GIT_COMMIT) {
    console.log(`[bundle-html] Using cached core bundle for commit ${GIT_COMMIT}`);
    onProgress({ stage: 'cache-hit', message: 'Using cached core files...' });
    return coreBundleCache;
  }
  
  console.log(`[bundle-html] Building core bundle for commit ${GIT_COMMIT}...`);
  const coreFiles = {};
  
  onProgress({ stage: 'discover', message: 'Discovering dependencies...' });
  
  // Discover all dependencies
  const allFiles = await discoverDependencies(acDir, ESSENTIAL_FILES, SKIP_FILES);
  
  onProgress({ stage: 'minify', message: `Minifying ${allFiles.length} files...` });
  
  // Load and minify files
  let minifiedCount = 0;
  for (const file of allFiles) {
    const fullPath = path.join(acDir, file);
    try {
      if (!fsSync.existsSync(fullPath)) continue;
      let content = await fs.readFile(fullPath, 'utf8');
      content = await minifyJS(content, file);
      coreFiles[file] = { content, binary: false, type: path.extname(file).slice(1) };
      minifiedCount++;
      // Progress update every 10 files
      if (minifiedCount % 10 === 0) {
        onProgress({ stage: 'minify', message: `Minified ${minifiedCount}/${allFiles.length} files...` });
      }
    } catch {
      // Skip files that can't be loaded
    }
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

// Main bundle creation
async function createBundle(pieceName, onProgress = () => {}) {
  const PIECE_NAME_NO_DOLLAR = pieceName.replace(/^\$/, '');
  const PIECE_NAME = '$' + PIECE_NAME_NO_DOLLAR;
  
  onProgress({ stage: 'fetch', message: `Fetching $${PIECE_NAME_NO_DOLLAR}...` });
  
  // Fetch KidLisp source with dependencies
  const { sources: kidlispSources, authorHandle } = await getKidLispSourceWithDeps(PIECE_NAME_NO_DOLLAR);
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
  });
  
  onProgress({ stage: 'compress', message: 'Compressing...' });
  
  // Create gzip-compressed self-extracting bundle
  const gzipCompressed = gzipSync(htmlContent, { level: 9 });
  const gzipBase64 = gzipCompressed.toString('base64');
  
  const finalHtml = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${PIECE_NAME} · Aesthetic Computer</title>
  <style>body{margin:0;background:#000;overflow:hidden}</style>
</head>
<body>
  <script>
    fetch('data:application/gzip;base64,${gzipBase64}')
      .then(r=>r.blob())
      .then(b=>b.stream().pipeThrough(new DecompressionStream('gzip')))
      .then(s=>new Response(s).text())
      .then(h=>{document.open();document.write(h);document.close();});
  </script>
</body>
</html>`;

  const filename = `$${PIECE_NAME_NO_DOLLAR}-${authorHandle}-${bundleTimestamp}.lisp.html`;
  
  return { html: finalHtml, filename, sizeKB: Math.round(finalHtml.length / 1024) };
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
  } = opts;

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${PIECE_NAME} · Aesthetic Computer</title>
  <script>
    // Console suppression
    (function() {
      const originalWarn = console.warn;
      const originalLog = console.log;
      const originalError = console.error;
      const originalInfo = console.info;
      
      const shouldSuppress = (...args) => {
        const fullMessage = args.map(a => {
          if (typeof a === 'string') return a;
          if (a instanceof Error) return a.message + a.stack;
          try { return String(a); } catch { return ''; }
        }).join(' ');
        
        return fullMessage.includes('WebGPU') ||
               fullMessage.includes('Implementation Status') ||
               fullMessage.includes('experimental on this platform') ||
               fullMessage.includes('gpuweb') ||
               fullMessage.includes('Context Provider') ||
               fullMessage.includes('viewpoint.svg') ||
               fullMessage.includes('VFS fetch:') ||
               fullMessage.includes('VFS miss:') ||
               fullMessage.includes('Boot completed') ||
               fullMessage.includes('🥾') ||
               fullMessage.includes('🟡 Error loading mjs') ||
               fullMessage.includes('Sending kidlisp-ready') ||
               fullMessage.includes('kidlisp-ready message') ||
               fullMessage.includes('hotSwap:') ||
               fullMessage.includes('typeof window:') ||
               fullMessage.includes('Response:') ||
               fullMessage.includes('🔍') ||
               fullMessage.includes('📤') ||
               fullMessage.includes('✅') ||
               fullMessage.includes('Initializing WebGPU') ||
               fullMessage.includes('🎨');
      };
      
      console.warn = function(...args) {
        if (shouldSuppress(...args)) return;
        return originalWarn.apply(console, args);
      };
      console.info = function(...args) {
        if (shouldSuppress(...args)) return;
        return originalInfo.apply(console, args);
      };
      console.log = function(...args) {
        if (shouldSuppress(...args)) return;
        return originalLog.apply(console, args);
      };
      console.error = function(...args) {
        if (shouldSuppress(...args)) return;
        return originalError.apply(console, args);
      };
    })();
  </script>
  <style>
    body { margin: 0; padding: 0; background: black; overflow: hidden; }
    canvas { display: block; }
  </style>
</head>
<body>
  <script type="module">
    window.acPACK_MODE = true;
    window.acKEEP_MODE = true;
    window.acSTARTING_PIECE = "${PIECE_NAME}";
    window.acPACK_PIECE = "${PIECE_NAME}";
    window.acPACK_DATE = "${packDate}";
    window.acPACK_GIT = "${gitVersion}";
    window.acKIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    
    // Don't set acPACK_DENSITY - let bios.mjs use its default logic
    
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
        fileCount: ${Object.keys(files).length}
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
        importMapEntries[filepath] = window.VFS_BLOB_URLS[filepath];
        importMapEntries['/' + filepath] = window.VFS_BLOB_URLS[filepath];
        importMapEntries[\`aesthetic.computer/\${filepath}\`] = window.VFS_BLOB_URLS[filepath];
        importMapEntries[\`/aesthetic.computer/\${filepath}\`] = window.VFS_BLOB_URLS[filepath];
        importMapEntries[\`./aesthetic.computer/\${filepath}\`] = window.VFS_BLOB_URLS[filepath];
        importMapEntries[\`https://aesthetic.computer/\${filepath}\`] = window.VFS_BLOB_URLS[filepath];
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
      
      vfsPath = vfsPath.replace(/^\\.\\.\\/+/g, '').replace(/^\\.\\//g, '').replace(/^\\//g, '');
      
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
    
    (async function() {
      if (window.acDecodePaintingsPromise) {
        await window.acDecodePaintingsPromise;
      }
      import(window.VFS_BLOB_URLS['boot.mjs']).catch(err => {
        console.error('Failed to load boot.mjs:', err);
      });
    })();
  </script>
</body>
</html>`;
}

// Main handler - uses Netlify streaming adapter
exports.handler = stream(async (event) => {
  const code = event.queryStringParameters?.code;
  const format = event.queryStringParameters?.format || 'html';
  
  if (!code) {
    return {
      statusCode: 400,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ error: "Missing 'code' parameter. Usage: /api/bundle-html?code=39j" }),
    };
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
          
          const { html, filename, sizeKB } = await createBundle(code, onProgress);
          
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
    
    const { html, filename, sizeKB } = await createBundle(code, onProgress);
    
    if (format === 'json' || format === 'base64') {
      return {
        statusCode: 200,
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          filename,
          content: Buffer.from(html).toString('base64'),
          sizeKB,
          progress: progressLog,
        }),
      };
    }
    
    // Default: return as downloadable HTML file
    return {
      statusCode: 200,
      headers: {
        "Content-Type": "text/html; charset=utf-8",
        "Content-Disposition": `attachment; filename="${filename}"`,
        "Cache-Control": "public, max-age=3600",
      },
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