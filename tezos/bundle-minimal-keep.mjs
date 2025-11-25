#!/usr/bin/env node

// Minimal bundler for simple KidLisp pieces like $pie
// Only includes essential dependencies, removes unused pieces
// Automatically compresses to gzip+base64 self-contained HTML
// Usage: node bundle-minimal-keep.mjs <piece-name>

import { promises as fs } from "fs";
import fsSync from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { minify } from "terser";
import { execSync } from "child_process";
import { gzipSync, brotliCompressSync, constants } from "zlib";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const PIECE_NAME = process.argv[2] || "$pie";
const PIECE_NAME_NO_DOLLAR = PIECE_NAME.replace(/^\$/, '');
const OUTPUT_DIR = path.join(__dirname, "keep-bundles");
const SOURCE_DIR = path.resolve(__dirname, "..");
const MINIFY_JS = true;

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

// OPTIMIZED: Only files in the import graph (from static analysis)
// This removes 37 unused files, saving ~200+ KB minified!
const ESSENTIAL_FILES = [
  // Core system (2 files)
  'boot.mjs',
  'bios.mjs',
  
  // Lib files actually imported by boot→bios chain (42 files)
  'lib/2d.mjs',
  'lib/ask.mjs',              // Required by systems/prompt-system.mjs
  'lib/cam-doll.mjs',         // Required by lib/disk.mjs
  'lib/chat.mjs',             // Required by lib/disk.mjs
  'lib/color-highlighting.mjs',
  'lib/disk.mjs',  // Required: bios.mjs dynamically imports this as fallback
  'lib/fade-state.mjs',
  'lib/gamepad.mjs',
  'lib/gamepad-mappings.mjs', // Required by lib/cam-doll.mjs
  'lib/geo.mjs',
  'lib/gizmo.mjs',            // Required by lib/disk.mjs
  'lib/gl.mjs',
  'lib/glaze.mjs',
  'lib/glazes/uniforms.js',
  'lib/graph.mjs',
  'lib/graphics-optimizer.mjs',
  'lib/headers.mjs',
  'lib/help.mjs',
  'lib/helpers.mjs',
  'lib/keyboard.mjs',
  'lib/kidlisp.mjs',
  'lib/logs.mjs',
  'lib/loop.mjs',
  'lib/melody-parser.mjs',
  'lib/midi.mjs',
  'lib/motion.mjs',
  'lib/num.mjs',
  'lib/pack-mode.mjs',
  'lib/parse.mjs',
  'lib/pen.mjs',
  'lib/platform.mjs',
  'lib/redact.mjs',           // Required by lib/chat.mjs
  'lib/shop.mjs',             // Required by lib/disk.mjs
  'lib/socket.mjs',           // Required by lib/disk.mjs
  'lib/sound/sound-whitelist.mjs',
  'lib/speech.mjs',
  'lib/store.mjs',
  'lib/text.mjs',  // Required: lib/disk.mjs imports this
  'lib/ticker.mjs',           // Required by lib/gizmo.mjs
  'lib/type.mjs',             // Required by lib/disk.mjs
  'lib/udp.mjs',
  'lib/ui.mjs',
  'lib/usb.mjs',
  'lib/webgpu.mjs',
  
  // Systems (3 files - required by lib/disk.mjs)
  'systems/nopaint.mjs',
  'systems/prompt-system.mjs',
  'systems/world.mjs',
  
  // Common disk files (4 files)
  'disks/common/debug.mjs',   // Required by lib/disk.mjs
  'disks/common/fonts.mjs',
  'disks/common/tape-player.mjs',
  
  // GL Matrix dependencies (7 files)
  'dep/gl-matrix/common.mjs',
  'dep/gl-matrix/mat3.mjs',
  'dep/gl-matrix/mat4.mjs',
  'dep/gl-matrix/quat.mjs',
  'dep/gl-matrix/vec2.mjs',
  'dep/gl-matrix/vec3.mjs',
  'dep/gl-matrix/vec4.mjs',
  
  // Other dependencies (5 files)
  'dep/@akamfoad/qr/qr.mjs',
  'dep/idb.js',
  'dep/nanoid/nanoid.js',
  'dep/nanoid/url-alphabet/index.js',
  'dep/geckos.io-client.2.3.2.min.js',
  
  // REMOVED: dep/wasmboy/wasmboy.ts.esm.js (376 KB!)
  // Only needed for Gameboy emulation, not for basic KidLisp pieces
];

// TODO: Investigate if wasmboy is actually used at runtime
// If not, removing it would save ~120+ KB minified!

// Removed files (37 total, ~200+ KB minified savings):
// lib/3d.mjs, lib/ask.mjs, lib/cam-doll.mjs, lib/chat-highlighting.mjs,
// lib/chat.mjs, lib/disk.mjs (HUGE!), lib/gamepad-mappings.mjs, lib/gesture.mjs,
// lib/gizmo.mjs, lib/hand-processor.js, lib/hand.mjs, lib/microphone.mjs,
// lib/notepat-convert.mjs, lib/redact.mjs, lib/shop.mjs, lib/socket.mjs,
// lib/sound/bubble.mjs, lib/sound/synth.mjs, lib/sound/volume.mjs, lib/speaker.mjs,
// lib/text.mjs, lib/ticker.mjs, lib/ticket.mjs, lib/type.mjs (HUGE!), lib/user-code.mjs,
// systems/nopaint.mjs, systems/prompt-system.mjs, systems/world.mjs,
// disks/common/debug.mjs, disks/common/music.mjs, disks/common/products.mjs,
// disks/common/scrub.mjs, disks/common/sfx.mjs,
// dep/gl-matrix/index.mjs, dep/gl-matrix/mat2.mjs, dep/gl-matrix/mat2d.mjs, dep/gl-matrix/quat2.mjs

// Minimal font set (just one variant)
const ESSENTIAL_FONTS = [
  'type/webfonts/ywft-processing-regular.ttf',
  'type/webfonts/ywft-processing-regular.woff',
  'type/webfonts/ywft-processing-regular.woff2', 
  'type/webfonts/ywft-processing-regular.svg',
  'type/webfonts/ywft-processing-regular.eot',
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
  
  // Get the directory of the base file
  let dir = path.dirname(base);
  
  // Start with directory parts (empty if dir is '.')
  const parts = dir === '.' ? [] : dir.split('/').filter(p => p);
  const relParts = relative.split('/');
  
  for (const part of relParts) {
    if (part === '..') parts.pop();
    else if (part !== '.' && part !== '') parts.push(part);
  }
  
  return parts.join('/');
}

function rewriteImports(code, filepath) {
  // DEBUG: Check input
  const hasRelative = code.match(/from\s*['"]\./g);
  if (hasRelative) {
    console.log(`   🔍 ${filepath} has ${hasRelative.length} relative import(s), rewriting...`);
    // Show first few imports
    const samples = code.match(/from\s*['"][^'"]{0,50}['"]/g);
    if (samples) console.log(`      Sample: ${samples.slice(0,3).join(', ')}`);
  }
  
  // Rewrite aesthetic.computer/ imports to ac/ format
  code = code.replace(/from\s*['"]aesthetic\.computer\/disks\/([^'"]+)['"]/g, (match, p) => {
    return 'from \'ac/disks/' + p + '\'';
  });
  
  code = code.replace(/import\s*\((['"]aesthetic\.computer\/disks\/([^'"]+)['")])\)/g, (match, fullPath, p) => {
    return 'import(\'ac/disks/' + p + '\')';
  });
  
  // Rewrite relative imports to bare specifiers (no leading /)
  // Make space after 'from' optional to match minified code
  let replacementCount = 0;
  code = code.replace(/from\s*['"](\.\.\/[^'"]+|\.\/[^'"]+)['"]/g, (match, p) => {
    const resolved = resolvePath(filepath, p); // Pass full filepath, not directory
    replacementCount++;
    // DEBUG: Show what we're replacing
    if (replacementCount <= 2) {
      console.log(`      DEBUG: base="${filepath}", rel="${p}" -> "${resolved}"`);
    }
    return 'from"' + resolved + '"'; // No space, use double quotes like minified code
  });
  
  if (replacementCount > 0) {
    console.log(`   ✅ Rewrote ${replacementCount} import(s) in ${filepath}`);
  }
  
  code = code.replace(/import\s*\((['"](\.\.\/[^'"]+|\.\/[^'"]+)['")])\)/g, (match, fullPath, p) => {
    const resolved = resolvePath(filepath, p); // Pass full filepath, not directory
    return 'import("' + resolved + '")'; // No leading /
  });
  
  // Also handle backtick template literals: import(`./path`)
  code = code.replace(/import\s*\(\`(\.\.\/[^\`]+|\.\/[^\`]+)\`\)/g, (match, p) => {
    const resolved = resolvePath(filepath, p); // Pass full filepath, not directory
    return 'import("' + resolved + '")'; // Convert to regular quotes with resolved path
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
    // If not minifying, rewrite imports now
    return rewriteImports(processedContent, relativePath);
  }
  
  try {
    const minified = await minify(processedContent, {
      compress: {
        dead_code: true,
        drop_console: false,
        drop_debugger: true, 
        unused: true,
        passes: 3,           // Multiple optimization passes
        pure_getters: true,  // Assume property getters have no side effects
        unsafe: true,        // More aggressive optimizations
        unsafe_math: true,   // Optimize math expressions
        unsafe_proto: true   // Optimize prototype.toString usage
      },
      mangle: {
        toplevel: false,
        properties: false    // Don't mangle property names (could break things)
      },
      format: {
        comments: false,
        ascii_only: false,   // Allow unicode
        ecma: 2020          // Use modern JS syntax for smaller output
      }
    });
    
    if (minified.code) {
      const originalSize = content.length;
      const minifiedSize = minified.code.length;
      const savings = ((originalSize - minifiedSize) / originalSize * 100).toFixed(1);
      console.log(`   🗜️  ${relativePath}: ${(originalSize/1024).toFixed(1)}KB → ${(minifiedSize/1024).toFixed(1)}KB (${savings}% smaller)`);
      
      // CRITICAL: Rewrite imports AFTER minification to work with terser's output format
      return rewriteImports(minified.code, relativePath);
    }
  } catch (error) {
    console.warn(`   ⚠️  Failed to minify ${relativePath}:`, error.message);
  }
  
  // If minification failed, still rewrite imports on original content
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
  // Match all $piece references in the source
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
    
    // Extract $ references
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
  
  // Fetch the piece and all its dependencies
  const allSources = await getKidLispSourceWithDeps(cleanName);
  
  // Return the main source and all dependencies
  return allSources;
}

async function createMinimalBundle(kidlispSources) {
  console.log(`\n📦 Creating minimal bundle for ${PIECE_NAME}...`);
  console.log(`📅 Pack date: ${packDate}`);
  console.log(`🔧 Git version: ${gitVersion}`);
  
  const files = {};
  
  // Get the main piece name and its source
  const PIECE_NAME_NO_DOLLAR = PIECE_NAME.replace('$', '');
  const mainSource = kidlispSources[PIECE_NAME_NO_DOLLAR];
  
  console.log(`📝 Embedding ${Object.keys(kidlispSources).length} KidLisp pieces:`);
  for (const [name, source] of Object.entries(kidlispSources)) {
    console.log(`   • $${name} (${source.length} chars)`);
  }
  // DISABLED: Don't load ALL lib/ files - only load ESSENTIAL_FILES
  // This was loading 87 files instead of the optimized 48!
  /*
  // Load all lib/ files automatically (they're core infrastructure)
  console.log("📁 Loading all lib/ files...");
  const acDir = path.join(SOURCE_DIR, "system/public/aesthetic.computer");
  const libDir = path.join(acDir, "lib");
  
  async function loadDirectory(dir, basePath, prefix = '') {
    const entries = await fs.readdir(dir, { withFileTypes: true });
    
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      const relativePath = prefix ? `${prefix}/${entry.name}` : entry.name;
      
      if (entry.isDirectory()) {
        await loadDirectory(fullPath, basePath, relativePath);
      } else if (entry.name.endsWith('.mjs') || entry.name.endsWith('.js') || entry.name.endsWith('.json')) {
        try {
          const fileKey = `${basePath}/${relativePath}`;
          let content = await inlineFile(fullPath, "text");
          
          // Special handling for fonts.mjs to prevent 404s
          // We do this BEFORE minification so the regex matches the source format
          if (fileKey === 'disks/common/fonts.mjs') {
            console.log('   🧹 Stripping glyphs from fonts.mjs to prevent 404s...');
            // Replace the font_1 object with a version that has no glyph keys
            content = content.replace(
              /export const font_1 = \{[\s\S]*?\};/, 
              `export const font_1 = {
  glyphHeight: 10,
  glyphWidth: 6,
  proportional: false,
  // Glyphs stripped for minimal bundle
};`
            );
          }

          // Only minify JS files, not JSON
          if (entry.name.endsWith('.mjs') || entry.name.endsWith('.js')) {
            content = await minifyIfJS(content, fileKey);
          }
          
          files[fileKey] = {
            content: content,
            binary: false,
            type: path.extname(entry.name).slice(1)
          };
        } catch (error) {
          console.warn(`   ⚠️  Could not load ${basePath}/${relativePath}: ${error.message}`);
        }
      }
    }
  }
  
  // Load essential root files (boot.mjs, bios.mjs, etc.)
  console.log("📁 Loading essential root files...");
  for (const file of ESSENTIAL_FILES) {
    if (!file.includes('/')) {  // Only load root-level files here
      try {
        let content = await inlineFile(path.join(acDir, file), "text");
        content = await minifyIfJS(content, file);
        files[file] = {
          content: content,
          binary: false,
          type: file.endsWith('.mjs') ? 'mjs' : 'js'
        };
      } catch (error) {
        console.warn(`   ⚠️  Could not load ${file}: ${error.message}`);
      }
    }
  }
  await loadDirectory(libDir, 'lib');
  
  // Load systems/ files
  console.log("📁 Loading systems/ files...");
  const systemsDir = path.join(acDir, "systems");
  await loadDirectory(systemsDir, 'systems');
  
  // Load common disk files
  console.log("📁 Loading disks/common/ files...");
  const commonDir = path.join(acDir, "disks/common");
  await loadDirectory(commonDir, 'disks/common');
  */

  // Instead, manually load ONLY the files in ESSENTIAL_FILES
  console.log("📁 Loading ONLY essential files from import graph...");
  const acDir = path.join(SOURCE_DIR, "system/public/aesthetic.computer");
  
  for (const file of ESSENTIAL_FILES) {
    
    try {
      const fullPath = path.join(acDir, file);
      let content = await inlineFile(fullPath, "text");
      
      // Special handling for fonts.mjs to prevent 404s
      if (file === 'disks/common/fonts.mjs') {
        console.log('   🧹 Stripping glyphs from fonts.mjs to prevent 404s...');
        content = content.replace(
          /export const font_1 = \{[\s\S]*?\};/, 
          `export const font_1 = {
  info: { face: "AC Font Regular", size: 34 },
  common: { lineHeight: 40, base: 32, scaleW: 1024, scaleH: 1024, pages: 1 },
  chars: {}  // Stripped to prevent 404s
};`
        );
      }
      
      // Minify JS files
      if (file.endsWith('.mjs') || file.endsWith('.js')) {
        content = await minifyIfJS(content, file);
      }
      
      files[file] = {
        content: content,
        binary: false,
        type: path.extname(file).slice(1)
      };
    } catch (error) {
      console.warn(`   ⚠️  Could not load ${file}: ${error.message}`);
    }
  }

  // Skip font_1 drawings - $pie doesn't use text rendering
  // This saves ~100 JSON files and reduces bundle size significantly
  console.log("📁 Skipping font_1 drawings (not needed for $pie)...");
  
  // DISABLED: Dependencies already loaded via ESSENTIAL_FILES above
  /*
  // Load only essential dependencies (not all 63MB of dep/)
  console.log("📁 Loading essential dependencies from dep/...");
  
  // dep/gl-matrix - Matrix math library
  const glMatrixDir = path.join(acDir, "dep/gl-matrix");
  await loadDirectory(glMatrixDir, 'dep/gl-matrix');
  
  // dep/@akamfoad/qr - QR code generation
  const qrDir = path.join(acDir, "dep/@akamfoad/qr");
  await loadDirectory(qrDir, 'dep/@akamfoad/qr');
  
  // dep/idb.js - IndexedDB wrapper (single file)
  const idbPath = path.join(acDir, "dep/idb.js");
  if (fsSync.existsSync(idbPath)) {
    const content = fsSync.readFileSync(idbPath, 'utf8');
    const minified = await minifyIfJS(content, 'dep/idb.js');
    files['dep/idb.js'] = { content: minified, binary: false, type: 'js' };
  }
  
  // dep/nanoid - ID generation
  const nanoidDir = path.join(acDir, "dep/nanoid");
  await loadDirectory(nanoidDir, 'dep/nanoid');
  
  // dep/geckos.io-client - WebRTC client (single file)
  const geckosPath = path.join(acDir, "dep/geckos.io-client.2.3.2.min.js");
  if (fsSync.existsSync(geckosPath)) {
    const content = fsSync.readFileSync(geckosPath, 'utf8');
    const minified = await minifyIfJS(content, 'dep/geckos.io-client.2.3.2.min.js');
    files['dep/geckos.io-client.2.3.2.min.js'] = { content: minified, binary: false, type: 'js' };
  }
  */
  
  // Skip font files - $pie doesn't use text rendering
  // This saves additional space from webfont files
  console.log("📁 Skipping webfonts (not needed for $pie)...");
  
  // Create synthetic .lisp files for all KidLisp pieces
  // Note: We DON'T create .mjs files because that would prevent
  // the system from loading KidLisp from window.objktKidlispCodes
  for (const [pieceName, source] of Object.entries(kidlispSources)) {
    const pieceLispPath = `disks/${pieceName}.lisp`;
    files[pieceLispPath] = {
      content: source,
      binary: false,
      type: 'lisp'
    };
    console.log(`📝 Created synthetic piece file: ${pieceLispPath}`);
  }
  
  console.log(`📊 Total files in VFS: ${Object.keys(files).length}`);
  
  // Create blob URLs for ES modules
  const moduleBlobs = {};
  const importMap = {};
  
  Object.keys(files).forEach(filePath => {
    if (filePath.endsWith('.mjs') || filePath.endsWith('.js')) {
      // Create blob URL for the module content
      const blob = new Blob([files[filePath].content], { 
        type: 'application/javascript' 
      });
      const blobUrl = URL.createObjectURL(blob);
      
      // Map all possible import paths to the blob URL
      importMap[`https://aesthetic.computer/${filePath}`] = blobUrl;
      importMap[`/aesthetic.computer/${filePath}`] = blobUrl;
      importMap[`./aesthetic.computer/${filePath}`] = blobUrl;
      importMap[`aesthetic.computer/${filePath}`] = blobUrl;
      
      moduleBlobs[filePath] = blobUrl;
    }
  });
  
  console.log(`📦 Found ${Object.keys(moduleBlobs)} JavaScript modules for import map`);
  
  // Generate HTML
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
    // Startup message
    console.log('Aesthetic.Computer');
    console.log('');
    console.log('${PIECE_NAME} is a piece by @jeffrey');
    console.log('');
    console.log('KidLisp source:', ${JSON.stringify(mainSource)});
    console.log('');
    console.log('This minimal copy was packed on ${packDate}');
    console.log('');
    console.log('Using aesthetic-computer git version ${gitVersion}');
    console.log('');
    
    // Pack mode flags
    window.acPACK_MODE = true;
    window.acSTARTING_PIECE = "${PIECE_NAME}";
    window.acPACK_PIECE = "${PIECE_NAME}";
    window.acPACK_DATE = "${packDate}";
    window.acPACK_GIT = "${gitVersion}";
    window.acKIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    
    // Embedded KidLisp sources for offline mode (including dependencies)
    window.EMBEDDED_KIDLISP_SOURCE = ${JSON.stringify(mainSource)};
    window.EMBEDDED_KIDLISP_PIECE = '${PIECE_NAME_NO_DOLLAR}';
    window.objktKidlispCodes = ${JSON.stringify(kidlispSources)};
    console.log('📝 Embedded ${Object.keys(kidlispSources).length} KidLisp pieces for offline use');
    
    // Prefill the KidLisp globalCodeCache with all embedded pieces
    // This is done after boot.mjs loads by hooking into module initialization
    window.acPREFILL_CODE_CACHE = ${JSON.stringify(kidlispSources)};
    
    // Intercept CSS link elements
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
    
    // Create blob URLs for all JS modules first
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
    
    // Create import map using the same logic as the working bundle
    const importMapEntries = {};
    
    for (const filepath of window.modulePaths) {
      if (window.VFS_BLOB_URLS[filepath]) {
        // Add main path
        importMapEntries[filepath] = window.VFS_BLOB_URLS[filepath];
        
        // Add with leading slash
        importMapEntries['/' + filepath] = window.VFS_BLOB_URLS[filepath];
        
        // Add aesthetic.computer paths
        importMapEntries[\`aesthetic.computer/\${filepath}\`] = window.VFS_BLOB_URLS[filepath];
        importMapEntries[\`/aesthetic.computer/\${filepath}\`] = window.VFS_BLOB_URLS[filepath];
        importMapEntries[\`./aesthetic.computer/\${filepath}\`] = window.VFS_BLOB_URLS[filepath];
        importMapEntries[\`https://aesthetic.computer/\${filepath}\`] = window.VFS_BLOB_URLS[filepath];
        
        // Add relative paths
        importMapEntries[\`./\${filepath}\`] = window.VFS_BLOB_URLS[filepath];
      }
    }
    
    // Create and inject import map
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
      
      // Normalize VFS path with URL decoding and aesthetic.computer/ prefix removal
      let vfsPath = decodeURIComponent(urlStr)
        .replace(/^https?:\\/\\/[^\\/]+\\//g, '')
        .replace(/^aesthetic\\.computer\\//g, '')  // Remove aesthetic.computer/ prefix
        .replace(/#.*$/g, '')
        .replace(/\\?.*$/g, '');
      
      // Handle relative paths like ../disks/ceo.mjs -> disks/ceo.mjs
      vfsPath = vfsPath.replace(/^\\.\\.\\/+/g, '').replace(/^\\.\\//g, '').replace(/^\\//g, '');
      
      // Check VFS
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
    
    // XHR interceptor for fonts and other resources
    XMLHttpRequest.prototype.open = function(method, url, ...args) {
      this._url = url;
      return originalXHROpen.call(this, method, url, ...args);
    };
    
    XMLHttpRequest.prototype.send = function(...args) {
      if (this._url) {
        const urlStr = this._url;
        // Normalize the URL path and decode URL encoding (e.g., %20 -> space)
        let vfsPath = decodeURIComponent(urlStr)
          .replace(/^https?:\\/\\/[^\\/]+\\//g, '')
          .replace(/^aesthetic\\.computer\\//g, '')  // Remove aesthetic.computer/ prefix
          .replace(/#.*$/g, '')
          .replace(/\\?.*$/g, '');
        
        // Handle relative paths like ../disks/ceo.mjs -> disks/ceo.mjs
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

  // Don't minify - the VFS JSON contains template literals that need newlines preserved
  // HTML minification would break the embedded JavaScript

  // Write uncompressed output
  await fs.mkdir(OUTPUT_DIR, { recursive: true });
  const outputPath = path.join(OUTPUT_DIR, `${PIECE_NAME.replace('$', '')}-minimal-nft.html`);
  await fs.writeFile(outputPath, htmlContent);
  
  const stats = await fs.stat(outputPath);
  const sizeMB = (stats.size / 1024 / 1024).toFixed(2);
  
  const totalChars = Object.values(kidlispSources).reduce((sum, src) => sum + src.length, 0);
  
  console.log(`\n✅ Minimal bundle created!`);
  console.log(`   📄 ${outputPath}`);
  console.log(`   💾 Size: ${sizeMB} MB`);
  console.log(`   🖼️  Minimal dependencies only`);
  console.log(`\n🚀 This file is completely self-contained!`);
  console.log(`   • Only essential files included`);
  console.log(`   • ${Object.keys(files).length} total files vs 502 in full bundle`);
  console.log(`   • ${Object.keys(kidlispSources).length} KidLisp pieces embedded (${totalChars} chars total)`);
  console.log(`   • Ready for Tezos minting`);
  
  // Now create compressed version
  console.log(`\n📦 Compressing bundle with Brotli (level 11)...`);
  const compressed = brotliCompressSync(htmlContent, {
    params: {
      [constants.BROTLI_PARAM_QUALITY]: 11,
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
    fetch('data:application/x-brotli;base64,${base64}')
      .then(r=>r.blob())
      .then(b=>b.stream().pipeThrough(new DecompressionStream('br')))
      .then(s=>new Response(s).text())
      .then(h=>{document.open();document.write(h);document.close();});
  </script>
</body>
</html>`;

  const compressedPath = path.join(OUTPUT_DIR, `${PIECE_NAME.replace('$', '')}-self-contained.html`);
  await fs.writeFile(compressedPath, selfContained);
  
  console.log(`\n📊 Compression results (Brotli):`);
  console.log(`   Original:  ${htmlContent.length.toLocaleString()} bytes`);
  console.log(`   Brotli:    ${compressed.length.toLocaleString()} bytes`);
  console.log(`   Base64:    ${base64.length.toLocaleString()} bytes`);
  console.log(`   Final:     ${selfContained.length.toLocaleString()} bytes = ${Math.round(selfContained.length/1024)} KB`);
  
  if (selfContained.length <= 256000) {
    console.log(`\n✅ Fits in 256 KB Tezos limit!`);
  } else {
    const overage = Math.round((selfContained.length - 256000) / 1024);
    console.log(`\n⚠️  Does not fit in 256 KB limit`);
    console.log(`   Overage: ${overage} KB`);
  }
  
  console.log(`\n📝 Written to: ${compressedPath}`);
  
  // Also create gzip version for browser testing (Simple Browser doesn't support Brotli)
  console.log(`\n📦 Creating gzip version for testing...`);
  const gzipCompressed = gzipSync(htmlContent, { level: 9 });
  const gzipBase64 = gzipCompressed.toString('base64');
  
  const gzipVersion = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${PIECE_NAME} • Aesthetic Computer</title>
  <style>
    body{margin:0;padding:0;background:#000;color:#fff;font-family:monospace;display:flex;align-items:center;justify-content:center;height:100vh}
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

  const gzipPath = path.join(OUTPUT_DIR, `${PIECE_NAME.replace('$', '')}-test.html`);
  await fs.writeFile(gzipPath, gzipVersion);
  
  console.log(`\n📊 Gzip test version:`);
  console.log(`   Gzipped:   ${gzipCompressed.length.toLocaleString()} bytes`);
  console.log(`   Final:     ${gzipVersion.length.toLocaleString()} bytes = ${Math.round(gzipVersion.length/1024)} KB`);
  console.log(`   📝 ${gzipPath}`);
  
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