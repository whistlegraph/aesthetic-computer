// KidLisp Bundler Library
// Shared bundle generation logic for CLI (ac-keep) and Netlify function
// Creates self-contained .lisp.html files for KidLisp pieces

import { promises as fs } from "fs";
import fsSync from "fs";
import path from "path";
import swc from "@swc/core";
import { execSync } from "child_process";
import { gzipSync } from "zlib";

// ============================================================================
// CONFIGURATION
// ============================================================================

const MINIFY_JS = true;

// Files to explicitly skip (saves significant space)
// Only skip files that are dynamically imported or truly optional
const SKIP_FILES = [
  // WasmBoy GameBoy emulator - 386 KB source, ~163 KB minified
  'dep/wasmboy/',
  
  // UDP/networking - 9 KB, only for multiplayer (dynamically imported)
  'lib/udp.mjs',
  'dep/geckos.io-client',
  
  // World system - 9.5 KB, 3D features
  'systems/world.mjs',
  
  // Graphics optimizer - runtime optimization (dynamically imported)
  'lib/graphics-optimizer.mjs',
];

// ULTRA-MINIMAL file set - only what's absolutely required for basic KidLisp visuals
const ESSENTIAL_FILES = [
  // Core system (required)
  'boot.mjs',
  'bios.mjs',
  
  // Core loop and disk
  'lib/loop.mjs',
  'lib/disk.mjs',
  'lib/parse.mjs',
  
  // KidLisp interpreter
  'lib/kidlisp.mjs',
  
  // Graphics essentials (2D only, NO 3D)
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
  
  // BIOS dependencies (required by imports)
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
  
  // Sound dependencies (tiny files)
  'lib/sound/sound-whitelist.mjs',
  
  // gl-matrix dependencies (used by geo, graph, disk, 2d)
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

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

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

function rewriteImports(code, filepath) {
  // Rewrite aesthetic.computer imports
  code = code.replace(/from\s*['"]aesthetic\.computer\/disks\/([^'"]+)['"]/g, (match, p) => {
    return 'from \'ac/disks/' + p + '\'';
  });
  
  code = code.replace(/import\s*\((['"]aesthetic\.computer\/disks\/([^'"]+)['")])\)/g, (match, fullPath, p) => {
    return 'import(\'ac/disks/' + p + '\')';
  });
  
  // Rewrite relative imports
  code = code.replace(/from\s*['"](\.\.\/[^'"]+|\.\/[^'"]+)['"]/g, (match, p) => {
    const resolved = resolvePath(filepath, p);
    return 'from"' + resolved + '"';
  });
  
  // Rewrite dynamic imports
  code = code.replace(/import\s*\((['"](\.\.\/[^'"]+|\.\/[^'"]+)['")])\)/g, (match, fullPath, p) => {
    const resolved = resolvePath(filepath, p);
    return 'import("' + resolved + '")';
  });
  
  code = code.replace(/import\s*\(\`(\.\.\/[^\`]+|\.\/[^\`]+)\`\)/g, (match, p) => {
    const resolved = resolvePath(filepath, p);
    return 'import("' + resolved + '")';
  });
  
  return code;
}

async function minifyJS(content, relativePath) {
  const ext = path.extname(relativePath);
  if (ext !== ".mjs" && ext !== ".js") {
    return content;
  }
  
  let processedContent = content;
  
  if (!MINIFY_JS) {
    return rewriteImports(processedContent, relativePath);
  }
  
  // Rewrite imports BEFORE minification
  processedContent = rewriteImports(processedContent, relativePath);
  
  try {
    // Use SWC for faster, better minification
    const result = await swc.minify(processedContent, {
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
        inline: 3,
        drop_console: true,
        drop_debugger: true
      },
      mangle: {
        toplevel: true,
        keep_classnames: false,
        keep_fnames: false,
        safari10: false
      },
      format: {
        comments: false,
        ascii_only: false
      },
      module: true,
      sourceMap: false
    });
    
    if (result.code) {
      return result.code;
    }
  } catch (error) {
    console.warn(`   ⚠️  Failed to minify ${relativePath}:`, error.message);
  }
  
  return processedContent;
}

// ============================================================================
// DEPENDENCY DISCOVERY
// ============================================================================

async function discoverDependencies(acDir, essentialFiles, skipFiles) {
  const discovered = new Set(essentialFiles);
  const toProcess = [...essentialFiles];
  
  while (toProcess.length > 0) {
    const file = toProcess.shift();
    const fullPath = path.join(acDir, file);
    
    if (!fsSync.existsSync(fullPath)) continue;
    
    try {
      const content = await fs.readFile(fullPath, 'utf8');
      
      // Find all relative imports
      const importRegex = /from\s+["'](\.\.[^"']+|\.\/[^"']+)["']/g;
      const dynamicImportRegex = /import\s*\(\s*["'](\.\.[^"']+|\.\/[^"']+)["']\s*\)/g;
      
      let match;
      while ((match = importRegex.exec(content)) !== null) {
        const importPath = match[1];
        const resolved = resolvePath(file, importPath);
        
        if (skipFiles.some(skip => resolved.includes(skip))) {
          continue;
        }
        
        if (!discovered.has(resolved)) {
          discovered.add(resolved);
          toProcess.push(resolved);
        }
      }
      
      while ((match = dynamicImportRegex.exec(content)) !== null) {
        const importPath = match[1];
        const resolved = resolvePath(file, importPath);
        
        if (skipFiles.some(skip => resolved.includes(skip))) {
          continue;
        }
        
        if (!discovered.has(resolved)) {
          discovered.add(resolved);
          toProcess.push(resolved);
        }
      }
    } catch (error) {
      // Ignore errors reading files
    }
  }
  
  return Array.from(discovered);
}

// ============================================================================
// KIDLISP SOURCE FETCHING
// ============================================================================

async function fetchKidLispFromAPI(pieceName, baseUrl = 'https://aesthetic.computer') {
  const cleanName = pieceName.replace('$', '');
  const url = `${baseUrl}/api/store-kidlisp?code=${cleanName}`;
  
  try {
    const response = await fetch(url);
    const data = await response.json();
    
    if (data.error || !data.source) {
      throw new Error(`Piece '$${cleanName}' not found`);
    }
    
    return data.source;
  } catch (error) {
    throw new Error(`Failed to fetch $${cleanName}: ${error.message}`);
  }
}

function extractKidLispRefs(source) {
  const refs = [];
  const regex = /\$[a-z0-9_-]+/gi;
  const matches = source.matchAll(regex);
  
  for (const match of matches) {
    const ref = match[0].toLowerCase();
    if (!refs.includes(ref)) {
      refs.push(ref);
    }
  }
  
  return refs;
}

export async function getKidLispSourceWithDeps(pieceName, baseUrl = 'https://aesthetic.computer') {
  const allSources = {};
  const toProcess = [pieceName];
  const processed = new Set();
  
  while (toProcess.length > 0) {
    const current = toProcess.shift();
    const cleanName = current.replace('$', '');
    
    if (processed.has(cleanName)) continue;
    processed.add(cleanName);
    
    const source = await fetchKidLispFromAPI(cleanName, baseUrl);
    allSources[cleanName] = source;
    
    const refs = extractKidLispRefs(source);
    for (const ref of refs) {
      const refName = ref.replace('$', '');
      if (!processed.has(refName)) {
        toProcess.push(refName);
      }
    }
  }
  
  return allSources;
}

// ============================================================================
// BUNDLE GENERATION
// ============================================================================

export async function createBundle(options) {
  const {
    pieceName,
    kidlispSources,
    acDir,
    authorHandle = '@jeffrey',
    verbose = false
  } = options;
  
  const PIECE_NAME_NO_DOLLAR = pieceName.replace('$', '');
  const PIECE_NAME = '$' + PIECE_NAME_NO_DOLLAR;
  const mainSource = kidlispSources[PIECE_NAME_NO_DOLLAR];
  
  // Get git info
  let gitHash = 'unknown';
  let gitDirty = '';
  let gitVersion = 'unknown';
  
  try {
    gitHash = execSync("git rev-parse --short HEAD", { encoding: "utf8", cwd: acDir }).trim();
    gitDirty = execSync("git diff --quiet || echo dirty", { encoding: "utf8", cwd: acDir }).trim();
    gitVersion = gitHash + (gitDirty ? " (dirty)" : "");
  } catch (e) {
    // Git might not be available
  }
  
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
  
  const files = {};
  
  // Auto-discover all dependencies from essential files
  if (verbose) console.log("🔍 Auto-discovering dependencies...");
  const allFiles = await discoverDependencies(acDir, ESSENTIAL_FILES, SKIP_FILES);
  if (verbose) console.log(`✅ Found ${allFiles.length} total files`);
  
  // Load all discovered files
  if (verbose) console.log("\n📁 Loading files...");
  
  for (const relativePath of allFiles) {
    const fullPath = path.join(acDir, relativePath);
    
    try {
      if (!fsSync.existsSync(fullPath)) {
        continue;
      }
      
      let content = await fs.readFile(fullPath, "utf8");
      content = await minifyJS(content, relativePath);
      
      files[relativePath] = {
        content: content,
        binary: false,
        type: path.extname(relativePath).slice(1)
      };
      
      if (verbose) console.log(`   ✅ Loaded: ${relativePath}`);
    } catch (error) {
      if (verbose) console.warn(`   ⚠️  Could not load ${relativePath}: ${error.message}`);
    }
  }
  
  // Create synthetic .lisp files for KidLisp pieces
  for (const [name, source] of Object.entries(kidlispSources)) {
    const pieceLispPath = `disks/${name}.lisp`;
    files[pieceLispPath] = {
      content: source,
      binary: false,
      type: 'lisp'
    };
  }

  // Generate filename for this bundle
  const now = new Date();
  const dateStr = `${now.getFullYear()}.${String(now.getMonth() + 1).padStart(2, '0')}.${String(now.getDate()).padStart(2, '0')}`;
  const timeStr = `${String(now.getHours()).padStart(2, '0')}${String(now.getMinutes()).padStart(2, '0')}`;
  const filename = `${PIECE_NAME_NO_DOLLAR}-${authorHandle}-${dateStr}-${timeStr}.lisp.html`;

  // Generate HTML
  const htmlContent = generateHTML({
    PIECE_NAME,
    PIECE_NAME_NO_DOLLAR,
    mainSource,
    kidlispSources,
    files,
    packDate,
    packTime,
    gitVersion,
    gitHash,
    gitDirty,
    authorHandle
  });
  
  // Create gzip-compressed self-extracting bundle
  const gzipCompressed = gzipSync(htmlContent, { level: 9 });
  const gzipBase64 = gzipCompressed.toString('base64');
  
  const selfExtractingHTML = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${PIECE_NAME} · Aesthetic Computer</title>
  <style>
    body{margin:0;background:#000;overflow:hidden;cursor:default}
  </style>
</head>
<body>
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
      .then(h=>{URL.revokeObjectURL(url);document.open();document.write(h);document.close();});
  </script>
</body>
</html>`;
  
  return {
    html: selfExtractingHTML,
    uncompressedHtml: htmlContent,
    pieceName: PIECE_NAME,
    filename,
    sizeKB: Math.round(selfExtractingHTML.length / 1024),
    uncompressedSizeKB: Math.round(htmlContent.length / 1024),
    fileCount: Object.keys(files).length
  };
}

function generateHTML(opts) {
  const {
    PIECE_NAME,
    PIECE_NAME_NO_DOLLAR,
    mainSource,
    kidlispSources,
    files,
    packDate,
    packTime,
    gitVersion,
    gitHash,
    gitDirty,
    authorHandle,
    filename
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
    body { margin: 0; padding: 0; background: black; overflow: hidden; cursor: default; }
    canvas { display: block; }
  </style>
</head>
<body>
  <script type="module">
    // Pack mode + KEEP mode flags
    window.acPACK_MODE = true;
    window.acKEEP_MODE = true;
    window.acSTARTING_PIECE = "${PIECE_NAME}";
    window.acPACK_PIECE = "${PIECE_NAME}";
    window.acPACK_DATE = "${packDate}";
    window.acPACK_GIT = "${gitVersion}";
    window.acPACK_URL = "https://aesthetic.computer/${PIECE_NAME}";
    window.acKIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    
    // Colophon data
    window.acPACK_COLOPHON = {
      piece: {
        name: '${PIECE_NAME}',
        sourceCode: ${JSON.stringify(mainSource)},
        isKidLisp: true
      },
      build: {
        author: '${authorHandle}',
        packTime: ${packTime},
        gitCommit: '${gitHash}',
        gitIsDirty: ${gitDirty ? 'true' : 'false'},
        fileCount: ${Object.keys(files).length},
        filename: '${filename}'
      }
    };
    
    // Embedded KidLisp sources
    window.EMBEDDED_KIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    window.EMBEDDED_KIDLISP_PIECE = '${PIECE_NAME_NO_DOLLAR}';
    window.objktKidlispCodes = ${JSON.stringify(kidlispSources)};
    window.acPREFILL_CODE_CACHE = ${JSON.stringify(kidlispSources)};
    
    // CSS link interception
    const originalAppendChild = Element.prototype.appendChild;
    Element.prototype.appendChild = function(child) {
      if (child.tagName === 'LINK' && child.rel === 'stylesheet' && child.href && child.href.includes('.css')) {
        return child;
      }
      return originalAppendChild.call(this, child);
    };
    
    // Virtual File System
    window.VFS = ${JSON.stringify(files).replace(/<\/script>/g, '<\\/script>')};
    
    // Create blob URLs
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
    
    // Create import map
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
    
    // VFS fetch interceptor
    const originalFetch = window.fetch;
    const originalXHROpen = XMLHttpRequest.prototype.open;
    const originalXHRSend = XMLHttpRequest.prototype.send;
    
    window.fetch = function(url, options) {
      const urlStr = typeof url === 'string' ? url : url.toString();
      
      let vfsPath = decodeURIComponent(urlStr)
        .replace(/^https?:\\/\\/[^\\/]+\\//g, '')
        .replace(/^aesthetic\\.computer\\//g, '')
        .replace(/#.*$/g, '')
        .replace(/\\?.*$/g, '');
      
      vfsPath = vfsPath.replace(/^\\.\\.\\/+/g, '').replace(/^\\.\\//g, '').replace(/^\\//g, '');
      
      if (window.VFS[vfsPath]) {
        const file = window.VFS[vfsPath];
        const content = file.binary ? atob(file.content) : file.content;
        
        return Promise.resolve(new Response(content, {
          status: 200,
          headers: { 'Content-Type': file.type === 'mjs' || file.type === 'js' ? 'application/javascript' : 'text/plain' }
        }));
      }
      
      // Silently handle expected missing files
      if (vfsPath.includes('disks/drawings/font_') || vfsPath.endsWith('.mjs') || vfsPath.includes('cursors/') || vfsPath.endsWith('.svg')) {
        return Promise.resolve(new Response('{}', { status: 404 }));
      }
      
      return originalFetch.call(this, url, options);
    };
    
    // XHR interceptor
    XMLHttpRequest.prototype.open = function(method, url, ...args) {
      this._url = url;
      return originalXHROpen.call(this, method, url, ...args);
    };
    
    XMLHttpRequest.prototype.send = function(...args) {
      if (this._url) {
        const urlStr = this._url;
        let vfsPath = decodeURIComponent(urlStr)
          .replace(/^https?:\\/\\/[^\\/]+\\//g, '')
          .replace(/^aesthetic\\.computer\\//g, '')
          .replace(/#.*$/g, '')
          .replace(/\\?.*$/g, '');
        
        vfsPath = vfsPath.replace(/^\\.\\.\\/+/g, '').replace(/^\\.\\//g, '').replace(/^\\//g, '');
        
        if (window.VFS[vfsPath]) {
          const file = window.VFS[vfsPath];
          
          Object.defineProperty(this, 'responseText', { writable: true, value: file.content });
          Object.defineProperty(this, 'response', { writable: true, value: file.content });
          Object.defineProperty(this, 'status', { writable: true, value: 200 });
          Object.defineProperty(this, 'readyState', { writable: true, value: 4 });
          
          setTimeout(() => {
            if (this.onreadystatechange) this.onreadystatechange();
            if (this.onload) this.onload();
          }, 0);
          
          return;
        }
        
        // Silently handle expected missing files
        if (vfsPath.includes('disks/drawings/font_') || vfsPath.endsWith('.mjs') || vfsPath.includes('cursors/') || vfsPath.endsWith('.svg')) {
          Object.defineProperty(this, 'responseText', { writable: true, value: '{}' });
          Object.defineProperty(this, 'response', { writable: true, value: '{}' });
          Object.defineProperty(this, 'status', { writable: true, value: 404 });
          Object.defineProperty(this, 'readyState', { writable: true, value: 4 });
          
          setTimeout(() => {
            if (this.onreadystatechange) this.onreadystatechange();
            if (this.onerror) this.onerror();
          }, 0);
          
          return;
        }
      }
      
      return originalXHRSend.call(this, ...args);
    };
    
    // Start the system
    import(window.VFS_BLOB_URLS['boot.mjs']).catch(err => {
      console.error('❌ Failed to load boot.mjs:', err);
    });
  </script>
</body>
</html>`;
}

// ============================================================================
// CACHING SUPPORT (for Netlify function)
// ============================================================================

let cachedVFS = null;
let cachedVFSGitHash = null;

export function clearVFSCache() {
  cachedVFS = null;
  cachedVFSGitHash = null;
  console.log('🗑️ VFS cache cleared');
}

export async function getCachedVFS(acDir) {
  let currentHash = 'unknown';
  try {
    // Run git from the workspace root, not acDir (which is inside public/)
    const workspaceRoot = path.resolve(acDir, '../../..');
    currentHash = execSync("git rev-parse HEAD", { encoding: "utf8", cwd: workspaceRoot }).trim();
  } catch (e) {
    currentHash = process.env.COMMIT_REF || process.env.BUILD_ID || 'unknown';
  }
  
  if (cachedVFS && cachedVFSGitHash === currentHash) {
    return { vfs: cachedVFS, fromCache: true };
  }
  
  // Build VFS
  console.log(`🔧 Building VFS from ${acDir}...`);
  const allFiles = await discoverDependencies(acDir, ESSENTIAL_FILES, SKIP_FILES);
  console.log(`📁 Discovered ${allFiles.length} files`);
  
  const vfs = {};
  
  for (const relativePath of allFiles) {
    const fullPath = path.join(acDir, relativePath);
    
    try {
      if (!fsSync.existsSync(fullPath)) {
        console.log(`   ⚠️ Missing: ${relativePath}`);
        continue;
      }
      
      let content = await fs.readFile(fullPath, "utf8");
      content = await minifyJS(content, relativePath);
      
      vfs[relativePath] = {
        content: content,
        binary: false,
        type: path.extname(relativePath).slice(1)
      };
    } catch (error) {
      console.log(`   ❌ Error: ${relativePath}: ${error.message}`);
    }
  }
  
  console.log(`✅ VFS built with ${Object.keys(vfs).length} files`);
  console.log(`   boot.mjs present: ${'boot.mjs' in vfs}`);
  
  cachedVFS = vfs;
  cachedVFSGitHash = currentHash;
  
  return { vfs: cachedVFS, fromCache: false };
}

export async function createBundleWithCache(options) {
  const {
    pieceName,
    kidlispSources,
    acDir,
    authorHandle = '@jeffrey'
  } = options;
  
  const PIECE_NAME_NO_DOLLAR = pieceName.replace('$', '');
  const PIECE_NAME = '$' + PIECE_NAME_NO_DOLLAR;
  const mainSource = kidlispSources[PIECE_NAME_NO_DOLLAR];
  
  // Get cached VFS
  const { vfs: cachedFiles, fromCache } = await getCachedVFS(acDir);
  
  // Clone and add piece-specific files
  const files = { ...cachedFiles };
  
  // Add KidLisp source files
  for (const [name, source] of Object.entries(kidlispSources)) {
    const pieceLispPath = `disks/${name}.lisp`;
    files[pieceLispPath] = {
      content: source,
      binary: false,
      type: 'lisp'
    };
  }
  
  // Get git info
  let gitHash = 'unknown';
  let gitDirty = '';
  let gitVersion = 'unknown';
  
  try {
    gitHash = execSync("git rev-parse --short HEAD", { encoding: "utf8", cwd: acDir }).trim();
    gitDirty = execSync("git diff --quiet || echo dirty", { encoding: "utf8", cwd: acDir }).trim();
    gitVersion = gitHash + (gitDirty ? " (dirty)" : "");
  } catch (e) {
    gitVersion = process.env.COMMIT_REF?.slice(0, 7) || 'unknown';
    gitHash = gitVersion;
  }
  
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
  
  // Generate HTML
  const htmlContent = generateHTML({
    PIECE_NAME,
    PIECE_NAME_NO_DOLLAR,
    mainSource,
    kidlispSources,
    files,
    packDate,
    packTime,
    gitVersion,
    gitHash,
    gitDirty,
    authorHandle
  });
  
  // Create gzip-compressed self-extracting bundle
  const gzipCompressed = gzipSync(htmlContent, { level: 9 });
  const gzipBase64 = gzipCompressed.toString('base64');
  
  const selfExtractingHTML = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${PIECE_NAME} · Aesthetic Computer</title>
  <style>
    body{margin:0;background:#000;overflow:hidden;cursor:default}
  </style>
</head>
<body>
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
      .then(h=>{URL.revokeObjectURL(url);document.open();document.write(h);document.close();});
  </script>
</body>
</html>`;
  
  return {
    html: selfExtractingHTML,
    uncompressedHtml: htmlContent,
    pieceName: PIECE_NAME,
    sizeKB: Math.round(selfExtractingHTML.length / 1024),
    uncompressedSizeKB: Math.round(htmlContent.length / 1024),
    fileCount: Object.keys(files).length,
    fromCache
  };
}
