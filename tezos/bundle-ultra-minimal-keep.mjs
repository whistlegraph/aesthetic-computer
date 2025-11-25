#!/usr/bin/env node

// Ultra-minimal bundler for simple KidLisp pieces
// Aggressively removes ALL unused systems to fit 256 KB Tezos limit
// Based on bundle-minimal-keep.mjs but with maximum pruning
// Usage: node bundle-ultra-minimal-keep.mjs <piece-name>

import { promises as fs } from "fs";
import fsSync from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { minify } from "terser";
import { execSync } from "child_process";
import { gzipSync, brotliCompressSync, constants } from "zlib";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const PIECE_NAME = process.argv[2] || "$wwi";
const PIECE_NAME_NO_DOLLAR = PIECE_NAME.replace(/^\$/, '');
const OUTPUT_DIR = path.join(__dirname, "keep-bundles");
const SOURCE_DIR = path.resolve(__dirname, "..");
const MINIFY_JS = true;

// Auto-discover dependencies by analyzing imports
async function discoverDependencies(acDir, essentialFiles, skipFiles) {
  const discovered = new Set(essentialFiles);
  const toProcess = [...essentialFiles];
  
  while (toProcess.length > 0) {
    const file = toProcess.shift();
    const fullPath = path.join(acDir, file);
    
    if (!fsSync.existsSync(fullPath)) continue;
    
    try {
      const content = await fs.readFile(fullPath, 'utf8');
      
      // Find all relative imports: from "./..." or from "../..."
      const importRegex = /from\s+["'](\.\.[^"']+|\.\/[^"']+)["']/g;
      const dynamicImportRegex = /import\s*\(\s*["'](\.\.[^"']+|\.\/[^"']+)["']\s*\)/g;
      
      let match;
      while ((match = importRegex.exec(content)) !== null) {
        const importPath = match[1];
        const resolved = resolvePath(file, importPath);
        
        // Skip if in skip list
        if (skipFiles.some(skip => resolved.includes(skip))) {
          continue;
        }
        
        if (!discovered.has(resolved)) {
          discovered.add(resolved);
          toProcess.push(resolved);
          console.log(`   🔍 Auto-discovered: ${resolved} (from ${file})`);
        }
      }
      
      while ((match = dynamicImportRegex.exec(content)) !== null) {
        const importPath = match[1];
        const resolved = resolvePath(file, importPath);
        
        // Skip if in skip list or dynamic import
        if (skipFiles.some(skip => resolved.includes(skip))) {
          continue;
        }
        
        if (!discovered.has(resolved)) {
          discovered.add(resolved);
          toProcess.push(resolved);
          console.log(`   🔍 Auto-discovered (dynamic): ${resolved} (from ${file})`);
        }
      }
    } catch (error) {
      // Ignore errors reading files
    }
  }
  
  return Array.from(discovered);
}

// Get git info
const gitHash = execSync("git rev-parse --short HEAD", { encoding: "utf8" }).trim();
const gitDirty = execSync("git diff --quiet || echo dirty", { encoding: "utf8" }).trim();
const gitVersion = gitHash + (gitDirty ? " (dirty)" : "");
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
  'lib/ui.mjs',  // Required by bios.mjs
  // 'lib/webgpu.mjs',  // Skip WebGPU to save ~1KB - not needed for 2D
  
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
  
  // REMOVED: Sound (save ~13 KB minified) - visual-only piece
  // REMOVED: headers.mjs, help.mjs (save ~10 KB) - not needed for embedded piece
];

// NO fonts - saves significant space for purely visual pieces
const ESSENTIAL_FONTS = [];

// Files to explicitly skip (systems that auto-load but aren't needed)
const SKIP_FILES = [
  // All systems are imported by disk.mjs, so we can't skip them
  // 'systems/world.mjs',  // NEEDED by disk.mjs
  // 'systems/prompt-system.mjs',  // NEEDED by disk.mjs import
  // 'systems/nopaint.mjs',  // NEEDED by disk.mjs import (even if not used)
  'lib/3d.mjs',
  'lib/hand.mjs',
  // 'lib/chat.mjs',  // NEEDED by disk.mjs
  // 'lib/socket.mjs',  // NEEDED by disk.mjs
  'lib/udp.mjs',
  'lib/microphone.mjs',
  // 'lib/cam-doll.mjs',  // NEEDED by disk.mjs
  'lib/type.mjs',  // Too large (100 KB) - skip for now
  'lib/gesture.mjs',    // Skip gesture - not in ESSENTIAL_FILES
  'disks/common/products.mjs',  // Only used by prompt.mjs which we skip
  
  // Skip heavy optional features
  'dep/wasmboy',        // Game Boy emulator
  'lib/graphics-optimizer.mjs',
  // 'dep/@akamfoad/qr',   // QR code generation - NEEDED by disk.mjs
  'disks/chat.mjs',
  'disks/common/fonts.mjs',  // Font loading
  'lib/ticker.mjs',
  'lib/redact.mjs',
  'lib/ask.mjs',
  'lib/gamepad-mappings.mjs',
  'lib/chat-highlighting.mjs',
  'disks/common/debug.mjs',
];

async function inlineFile(filePath, type = "text") {
  try {
    if (type === "binary") {
      const buffer = await fs.readFile(filePath);
      return buffer.toString("base64");
    }
    return await fs.readFile(filePath, "utf8");
  } catch (error) {
    console.warn(`⚠️ Could not read ${filePath}: ${error.message}`);
    return "";
  }
}

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
  const hasRelative = code.match(/from\s*['"]\./g);
  if (hasRelative) {
    console.log(`   🔍 ${filepath} has ${hasRelative.length} relative import(s), rewriting...`);
  }
  
  code = code.replace(/from\s*['"]aesthetic\.computer\/disks\/([^'"]+)['"]/g, (match, p) => {
    return 'from \'ac/disks/' + p + '\'';
  });
  
  code = code.replace(/import\s*\((['"]aesthetic\.computer\/disks\/([^'"]+)['")])\)/g, (match, fullPath, p) => {
    return 'import(\'ac/disks/' + p + '\')';
  });
  
  let replacementCount = 0;
  code = code.replace(/from\s*['"](\.\.\/[^'"]+|\.\/[^'"]+)['"]/g, (match, p) => {
    const resolved = resolvePath(filepath, p);
    replacementCount++;
    return 'from"' + resolved + '"';
  });
  
  if (replacementCount > 0) {
    console.log(`   ✅ Rewrote ${replacementCount} import(s) in ${filepath}`);
  }
  
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

async function minifyIfJS(content, relativePath) {
  const ext = path.extname(relativePath);
  if (ext !== ".mjs" && ext !== ".js") {
    return content;
  }
  
  let processedContent = content;
  
  if (!MINIFY_JS) {
    return rewriteImports(processedContent, relativePath);
  }
  
  try {
    const minified = await minify(processedContent, {
      compress: {
        dead_code: true,
        drop_console: false,
        drop_debugger: true, 
        unused: true,
        passes: 5,  // Increased from 3 to 5 for better compression
        pure_getters: true,
        unsafe: true,
        unsafe_math: true,
        unsafe_proto: true,
        unsafe_comps: true,
        unsafe_Function: true,
        unsafe_regexp: true,
        unsafe_undefined: true,
        collapse_vars: true,
        reduce_vars: true,
        inline: 3,  // Aggressive inlining
        join_vars: true,
        sequences: true,
        evaluate: true,
        conditionals: true,
        booleans: true,
        loops: true,
        side_effects: true
      },
      mangle: {
        toplevel: true,  // Enable toplevel mangling for better compression
        properties: false
      },
      format: {
        comments: false,
        ascii_only: false,
        ecma: 2020
      }
    });
    
    if (minified.code) {
      const originalSize = content.length;
      const minifiedSize = minified.code.length;
      const savings = ((originalSize - minifiedSize) / originalSize * 100).toFixed(1);
      console.log(`   🗜️  ${relativePath}: ${(originalSize/1024).toFixed(1)}KB → ${(minifiedSize/1024).toFixed(1)}KB (${savings}% smaller)`);
      
      return rewriteImports(minified.code, relativePath);
    }
  } catch (error) {
    console.warn(`   ⚠️  Failed to minify ${relativePath}:`, error.message);
  }
  
  return rewriteImports(processedContent, relativePath);
}

async function fetchKidLispFromAPI(pieceName) {
  const cleanName = pieceName.replace('$', '');
  const url = `https://aesthetic.computer/api/store-kidlisp?code=${cleanName}`;
  
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

async function getKidLispSourceWithDeps(pieceName) {
  console.log(`\n🔍 Resolving KidLisp dependencies for ${pieceName}...`);
  
  const allSources = {};
  const toProcess = [pieceName];
  const processed = new Set();
  
  while (toProcess.length > 0) {
    const current = toProcess.shift();
    const cleanName = current.replace('$', '');
    
    if (processed.has(cleanName)) continue;
    processed.add(cleanName);
    
    console.log(`   📥 Fetching: $${cleanName}`);
    const source = await fetchKidLispFromAPI(cleanName);
    allSources[cleanName] = source;
    
    const refs = extractKidLispRefs(source);
    if (refs.length > 0) {
      console.log(`      └─ Found refs: ${refs.join(', ')}`);
      for (const ref of refs) {
        const refName = ref.replace('$', '');
        if (!processed.has(refName)) {
          toProcess.push(refName);
        }
      }
    }
  }
  
  console.log(`✅ Resolved ${Object.keys(allSources).length} KidLisp pieces total`);
  return allSources;
}

async function getKidLispSource(pieceName) {
  const cleanName = pieceName.replace('$', '');
  const allSources = await getKidLispSourceWithDeps(cleanName);
  return allSources;
}

async function createMinimalBundle(kidlispSources) {
  console.log(`\n📦 Creating ULTRA-MINIMAL bundle for ${PIECE_NAME}...`);
  console.log(`📅 Pack date: ${packDate}`);
  console.log(`🔧 Git version: ${gitVersion}`);
  console.log(`🎯 Target: < 256 KB for Tezos storage`);
  
  const files = {};
  
  const PIECE_NAME_NO_DOLLAR = PIECE_NAME.replace('$', '');
  const mainSource = kidlispSources[PIECE_NAME_NO_DOLLAR];
  
  console.log(`📝 Embedding ${Object.keys(kidlispSources).length} KidLisp pieces:`);
  for (const [name, source] of Object.entries(kidlispSources)) {
    console.log(`   • $${name} (${source.length} chars)`);
  }
  
  const acDir = path.join(SOURCE_DIR, "system/public/aesthetic.computer");
  
  // Auto-discover all dependencies from essential files
  console.log("🔍 Auto-discovering dependencies...");
  const allFiles = await discoverDependencies(acDir, ESSENTIAL_FILES, SKIP_FILES);
  console.log(`✅ Found ${allFiles.length} total files (${ESSENTIAL_FILES.length} essential + ${allFiles.length - ESSENTIAL_FILES.length} dependencies)`);
  
  // Load ONLY essential files (no automatic directory scanning)
  console.log("\n📁 Loading all discovered files...");
  
  async function loadFileIfNeeded(relativePath, skipCheck = true) {
    // Only skip if requested AND in skip list
    // Note: auto-discovered files bypass skip check since they're needed
    if (skipCheck && SKIP_FILES.some(skip => relativePath.includes(skip))) {
      console.log(`   ⏭️  Skipping: ${relativePath}`);
      return;
    }
    
    const fullPath = path.join(acDir, relativePath);
    
    try {
      if (!fsSync.existsSync(fullPath)) {
        console.warn(`   ⚠️  File not found: ${relativePath}`);
        return;
      }
      
      let content = await inlineFile(fullPath, "text");
      
      content = await minifyIfJS(content, relativePath);
      
      files[relativePath] = {
        content: content,
        binary: false,
        type: path.extname(relativePath).slice(1)
      };
      
      console.log(`   ✅ Loaded: ${relativePath}`);
    } catch (error) {
      console.warn(`   ⚠️  Could not load ${relativePath}: ${error.message}`);
    }
  }
  
  // Load each discovered file (don't skip - they were filtered during discovery)
  for (const file of allFiles) {
    await loadFileIfNeeded(file, false); // false = don't skip, these are needed
  }
  
  // Load ONLY essential dependencies (very selective)
  console.log("📁 Loading MINIMAL dependencies...");
  
  // gl-matrix (only if we're using 3D - we're not, so skip)
  // nanoid (might be used for IDs)
  const nanoidPath = 'dep/nanoid/index.js';
  await loadFileIfNeeded(nanoidPath);
  
  // Skip idb.js if not needed
  // Skip geckos.io (networking not needed)
  // Skip qr code generation
  // Skip Three.js entirely
  
  // Load ONLY minimal font (just woff2)
  console.log("📁 Loading MINIMAL fonts (woff2 only)...");
  for (const font of ESSENTIAL_FONTS) {
    const fullPath = path.join(acDir, font);
    try {
      if (fsSync.existsSync(fullPath)) {
        const buffer = await fs.readFile(fullPath);
        const base64 = buffer.toString("base64");
        files[font] = {
          content: base64,
          binary: true,
          type: 'woff2'
        };
        console.log(`   ✅ Loaded: ${font} (${Math.round(buffer.length/1024)} KB)`);
      }
    } catch (error) {
      console.warn(`   ⚠️  Could not load ${font}: ${error.message}`);
    }
  }
  
  // Create synthetic .lisp files for KidLisp pieces
  for (const [pieceName, source] of Object.entries(kidlispSources)) {
    const pieceLispPath = `disks/${pieceName}.lisp`;
    files[pieceLispPath] = {
      content: source,
      binary: false,
      type: 'lisp'
    };
    console.log(`📝 Created synthetic piece file: ${pieceLispPath}`);
  }
  
  console.log(`\n📊 ULTRA-MINIMAL bundle stats:`);
  console.log(`   Total files: ${Object.keys(files).length}`);
  console.log(`   vs previous: 87 files`);
  console.log(`   Reduction: ${Math.round((1 - Object.keys(files).length / 87) * 100)}%`);
  
  // Generate HTML with KEEP mode flag
  let htmlContent = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${PIECE_NAME} • Aesthetic Computer</title>
  <style>
    body { margin: 0; padding: 0; background: black; overflow: hidden; }
    canvas { display: block; }
  </style>
</head>
<body>
  <script type="module">
    console.log('Aesthetic.Computer');
    console.log('');
    console.log('${PIECE_NAME} is a piece by @jeffrey');
    console.log('');
    console.log('KidLisp source:', ${JSON.stringify(mainSource)});
    console.log('');
    console.log('This ULTRA-MINIMAL copy was packed on ${packDate}');
    console.log('');
    console.log('Using aesthetic-computer git version ${gitVersion}');
    console.log('');
    
    // Pack mode + KEEP mode flags
    window.acPACK_MODE = true;
    window.acKEEP_MODE = true;  // NEW: Signal ultra-minimal mode
    window.acSTARTING_PIECE = "${PIECE_NAME}";
    window.acPACK_PIECE = "${PIECE_NAME}";
    window.acPACK_DATE = "${packDate}";
    window.acPACK_GIT = "${gitVersion}";
    window.acKIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    
    // Embedded KidLisp sources
    window.EMBEDDED_KIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    window.EMBEDDED_KIDLISP_PIECE = '${PIECE_NAME_NO_DOLLAR}';
    window.objktKidlispCodes = ${JSON.stringify(kidlispSources)};
    window.acPREFILL_CODE_CACHE = ${JSON.stringify(kidlispSources)};
    
    console.log('📝 Embedded ${Object.keys(kidlispSources).length} KidLisp pieces');
    console.log('🎯 KEEP MODE: Ultra-minimal bundle for Tezos');
    
    // Intercept CSS links
    const originalAppendChild = Element.prototype.appendChild;
    Element.prototype.appendChild = function(child) {
      if (child.tagName === 'LINK' && child.rel === 'stylesheet' && child.href && child.href.includes('.css')) {
        console.log('🚫 Blocked CSS link:', child.href);
        return child;
      }
      return originalAppendChild.call(this, child);
    };
    
    const originalBodyAppend = HTMLBodyElement.prototype.append;
    HTMLBodyElement.prototype.append = function(...nodes) {
      const filteredNodes = nodes.filter(node => {
        if (node.tagName === 'LINK' && node.rel === 'stylesheet') {
          console.log('🚫 Blocked CSS link via body.append:', node.href);
          return false;
        }
        return true;
      });
      return originalBodyAppend.call(this, ...filteredNodes);
    };
    console.log('✅ CSS link blocking installed');
    
    // Virtual File System
    window.VFS = ${JSON.stringify(files).replace(/<\/script>/g, '<\\/script>')};
    
    // Create blob URLs
    window.VFS_BLOB_URLS = {};
    window.modulePaths = [];
    
    Object.entries(window.VFS).forEach(([path, file]) => {
      if (path.endsWith('.mjs') || path.endsWith('.js')) {
        const blob = new Blob([file.content], { 
          type: 'application/javascript' 
        });
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
    
    console.log(\`✅ Import map created with \${Object.keys(importMapEntries).length} entries\`);
    
    // VFS fetch/XHR interceptor
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
        console.log('✓ VFS fetch:', vfsPath);
        
        return Promise.resolve(new Response(content, {
          status: 200,
          headers: { 'Content-Type': file.type === 'mjs' || file.type === 'js' ? 'application/javascript' : 'text/plain' }
        }));
      }
      
      console.log('VFS miss:', vfsPath);
      return originalFetch.call(this, url, options);
    };
    
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
          
          console.log('✓ XHR VFS:', vfsPath);
          
          setTimeout(() => {
            if (this.onreadystatechange) this.onreadystatechange();
            if (this.onload) this.onload();
          }, 0);
          
          return;
        }
      }
      
      return originalXHRSend.call(this, ...args);
    };
    
    console.log('✅ VFS fetch/XHR interceptors installed');
    
    // Start the system  
    console.log('🚀 Loading boot.mjs...');
    import(window.VFS_BLOB_URLS['boot.mjs']).catch(err => {
      console.error('❌ Failed to load boot.mjs:', err);
    });
  </script>
</body>
</html>`;

  // Write uncompressed output
  await fs.mkdir(OUTPUT_DIR, { recursive: true });
  const outputPath = path.join(OUTPUT_DIR, `${PIECE_NAME.replace('$', '')}-ultra-minimal-nft.html`);
  await fs.writeFile(outputPath, htmlContent);
  
  const stats = await fs.stat(outputPath);
  const sizeKB = (stats.size / 1024).toFixed(2);
  
  console.log(`\n✅ ULTRA-MINIMAL bundle created!`);
  console.log(`   📄 ${outputPath}`);
  console.log(`   💾 Size: ${sizeKB} KB (uncompressed)`);
  
  // Compress with Brotli (much better than gzip!)
  console.log(`\n📦 Compressing bundle with Brotli (level 11)...`);
  const compressed = brotliCompressSync(htmlContent, {
    params: {
      [constants.BROTLI_PARAM_QUALITY]: 11, // Maximum quality
      [constants.BROTLI_PARAM_SIZE_HINT]: htmlContent.length
    }
  });
  const base64 = compressed.toString('base64');
  
  const selfContained = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${PIECE_NAME} • Aesthetic Computer</title>
  <style>
    body{margin:0;background:#000;overflow:hidden}
  </style>
</head>
<body>
  <script>
    fetch('data:application/octet-stream;base64,${base64}')
      .then(r=>r.blob())
      .then(b=>b.stream().pipeThrough(new DecompressionStream('br')))
      .then(s=>new Response(s).text())
      .then(h=>{document.open();document.write(h);document.close();});
  </script>
</body>
</html>`;

  const compressedPath = path.join(OUTPUT_DIR, `${PIECE_NAME.replace('$', '')}-ultra-self-contained.html`);
  await fs.writeFile(compressedPath, selfContained);
  
  const finalSizeKB = Math.round(selfContained.length / 1024);
  const tezosLimitKB = 256;
  
  console.log(`\n📊 Compression results (Brotli):`);
  console.log(`   Original:   ${htmlContent.length.toLocaleString()} bytes`);
  console.log(`   Brotli:     ${compressed.length.toLocaleString()} bytes`);
  console.log(`   Base64:     ${base64.length.toLocaleString()} bytes`);
  console.log(`   Final:      ${selfContained.length.toLocaleString()} bytes = ${finalSizeKB} KB`);
  console.log(`   Target:     ${tezosLimitKB} KB`);
  console.log(`   vs gzip:    ~${Math.round((200 - compressed.length/1024))} KB smaller!`);
  
  if (selfContained.length <= 256000) {
    const headroom = tezosLimitKB - finalSizeKB;
    console.log(`\n✅ ✅ ✅ FITS IN 256 KB TEZOS LIMIT! ✅ ✅ ✅`);
    console.log(`   Headroom: ${headroom} KB`);
  } else {
    const overage = finalSizeKB - tezosLimitKB;
    console.log(`\n⚠️  Still over by ${overage} KB`);
    console.log(`   Need to remove ${Math.ceil(overage)} more KB of code`);
  }
  
  console.log(`\n📝 Written to: ${compressedPath}`);
  
  // Also create gzip version for browser testing (VS Code Simple Browser doesn't support Brotli)
  console.log(`\n📦 Creating gzip version for browser testing...`);
  const gzipCompressed = gzipSync(htmlContent, { level: 9 });
  const gzipBase64 = gzipCompressed.toString('base64');
  
  const gzipSelfContained = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${PIECE_NAME} • Aesthetic Computer</title>
  <style>
    body{margin:0;background:#000;overflow:hidden}
  </style>
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

  const gzipPath = path.join(OUTPUT_DIR, `${PIECE_NAME.replace('$', '')}-ultra-gzip.html`);
  await fs.writeFile(gzipPath, gzipSelfContained);
  
  const gzipSizeKB = Math.round(gzipSelfContained.length / 1024);
  console.log(`   Gzip version: ${gzipSizeKB} KB (for browser testing)`);
  console.log(`   Written to: ${gzipPath}`);
  
  return compressedPath;
}

async function main() {
  try {
    const kidlispSources = await getKidLispSource(PIECE_NAME);
    await createMinimalBundle(kidlispSources);
  } catch (error) {
    console.error("❌ Bundle creation failed:", error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
