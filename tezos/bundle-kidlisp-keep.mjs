#!/usr/bin/env node

// Tezos Keep Bundler - Creates single-file HTML with embedded KidLisp source
// Based on ac-pack-single-file.mjs but fetches and embeds KidLisp source at build time
// Usage: node bundle-kidlisp-keep.mjs <kidlisp-piece>
// Example: node bundle-kidlisp-keep.mjs ceo

import { promises as fs } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { execSync } from "child_process";
import https from "https";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const PIECE_NAME = process.argv[2] || "ceo";
const OUTPUT_DIR = path.join(__dirname, "keep-bundles");
const SOURCE_DIR = path.resolve(__dirname, "..");

// Get git info
const gitHash = execSync("git rev-parse --short HEAD", { encoding: "utf8" }).trim();
const gitDirty = execSync("git diff --quiet || echo dirty", { encoding: "utf8" }).trim();
const gitVersion = gitHash + (gitDirty ? " (dirty)" : "");
const packDate = new Date().toLocaleString("en-US", {
  month: "long",
  day: "numeric", 
  year: "numeric",
  hour: "numeric",
  minute: "2-digit",
  second: "2-digit"
});

async function fetchKidLispSource(piece) {
  console.log(`📡 Fetching KidLisp source for: ${piece}...`);
  
  const url = `https://aesthetic.computer/.netlify/functions/store-kidlisp?code=${piece}`;
  
  return new Promise((resolve, reject) => {
    https.get(url, (res) => {
      let data = '';
      res.on('data', (chunk) => data += chunk);
      res.on('end', () => {
        try {
          const json = JSON.parse(data);
          if (json.source) {
            console.log(`✅ Source: ${json.source.substring(0, 100)}...`);
            resolve(json.source);
          } else {
            reject(new Error('No source found in response'));
          }
        } catch (e) {
          reject(e);
        }
      });
    }).on('error', reject);
  });
}

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

async function inlineDirectory(dirPath, basePath = dirPath, excludeImages = false) {
  const files = {};
  const entries = await fs.readdir(dirPath, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = path.join(dirPath, entry.name);
    const relativePath = path.relative(basePath, fullPath);

    if (entry.isDirectory()) {
      const subFiles = await inlineDirectory(fullPath, basePath, excludeImages);
      Object.assign(files, subFiles);
    } else {
      const ext = path.extname(entry.name).toLowerCase();
      const isBinary = [".png", ".gif", ".jpg", ".jpeg", ".ico", ".woff", ".woff2", ".ttf", ".otf"].includes(ext);
      const isImage = [".png", ".gif", ".jpg", ".jpeg", ".ico"].includes(ext);
      
      if (excludeImages && isImage) continue;
      
      files[relativePath] = {
        content: await inlineFile(fullPath, isBinary ? "binary" : "text"),
        binary: isBinary,
        type: ext.slice(1) || "txt"
      };
    }
  }

  return files;
}

async function createKeepBundle(pieceName, kidlispSource) {
  console.log(`\n📦 Creating keep bundle for $${pieceName}...`);
  console.log(`📅 Date: ${packDate}`);
  console.log(`🔧 Git: ${gitVersion}`);

  // Inline essential files
  console.log("\n📁 Inlining ac/ directory...");
  const acDir = path.join(SOURCE_DIR, "system/public/aesthetic.computer");
  const essentialDirs = ["boot.mjs", "bios.mjs", "lib", "disks", "systems", "style.css", "cursors"];
  
  const acFiles = {};
  for (const item of essentialDirs) {
    const itemPath = path.join(acDir, item);
    const stats = await fs.stat(itemPath).catch(() => null);
    
    if (!stats) continue;
    
    if (stats.isDirectory()) {
      const dirFiles = await inlineDirectory(itemPath, acDir, false);
      Object.assign(acFiles, dirFiles);
    } else {
      const relativePath = path.relative(acDir, itemPath);
      const ext = path.extname(item).toLowerCase();
      const isBinary = [".png", ".gif", ".jpg", ".jpeg", ".ico", ".woff", ".woff2", ".ttf", ".otf"].includes(ext);
      acFiles[relativePath] = {
        content: await inlineFile(itemPath, isBinary ? "binary" : "text"),
        binary: isBinary,
        type: ext.slice(1) || "txt"
      };
    }
  }
  
  // Add dependencies
  console.log("📁 Inlining dependencies...");
  const deps = [
    "dep/gl-matrix",
    "dep/@akamfoad/qr",
    "dep/idb.js",
    "dep/nanoid",
    "dep/geckos.io-client.2.3.2.min.js"
  ];
  
  for (const dep of deps) {
    const depPath = path.join(acDir, dep);
    const stats = await fs.stat(depPath).catch(() => null);
    if (!stats) continue;
    
    if (stats.isDirectory()) {
      const depFiles = await inlineDirectory(depPath, acDir, true);
      Object.assign(acFiles, depFiles);
    } else {
      const relativePath = path.relative(acDir, depPath);
      acFiles[relativePath] = {
        content: await inlineFile(depPath, "text"),
        binary: false,
        type: "js"
      };
    }
  }
  
  console.log(`   ✓ Found ${Object.keys(acFiles).length} files`);

  // Inline type/ directory
  console.log("📁 Inlining type/ directory (fonts & CSS)...");
  const typeDir = path.join(SOURCE_DIR, "system/public/type");
  const typeFiles = await inlineDirectory(typeDir, typeDir, false);
  console.log(`   ✓ Found ${Object.keys(typeFiles).length} files`);

  // Inline CSS files directly into style tags (skip font CSS since we don't need it)
  const cssFiles = Object.entries(typeFiles).filter(([k, v]) => k.endsWith('.css'));
  let inlinedCSS = "";
  for (const [filepath, filedata] of cssFiles) {
    // Skip webfont CSS files - we don't need @font-face rules for the bundled version
    if (filepath.includes('webfonts/')) {
      continue;
    }
    inlinedCSS += `\n/* ${filepath} */\n${filedata.content}\n`;
  }

  // Create VFS
  const vfs = {
    ...Object.fromEntries(Object.entries(acFiles).map(([k, v]) => [`ac/${k}`, v])),
    ...Object.fromEntries(Object.entries(typeFiles).filter(([k]) => !k.endsWith('.css')).map(([k, v]) => [`type/${k}`, v]))
  };

  console.log(`\n📊 Total files in VFS: ${Object.keys(vfs).length}`);
  console.log(`📝 Inline CSS: ${cssFiles.length} files`);

  // Sanitize server-side template placeholders that would be left as literals in the bundle
  // Many source files contain templates like ${getSrc(record)} which were meant to be
  // interpolated server-side. Remove those expressions so the client doesn't show raw
  // template strings or request malformed URLs (we'll leave conservative removals only).
  for (const [p, file] of Object.entries(vfs)) {
    if (!file || file.binary) continue;
    // Replace ${...} template expressions with empty string
    file.content = file.content.replace(/\$\{[^}]+\}/g, '');
    // Also remove stray double-quote encodings that sometimes wrap templates ("${...}")
    file.content = file.content.replace(/%22\$\{[^}]+\}%22/g, '');
  }

  // Find all JS modules for import map
  const modulePaths = Object.keys(vfs).filter(p => 
    p.endsWith('.js') || p.endsWith('.mjs')
  );
  console.log(`📦 Found ${modulePaths.length} JavaScript modules`);

  // Generate HTML
  // Escape closing </script> sequences so embedded JSON doesn't terminate the script block
  const vfsJson = JSON.stringify(vfs).replace(/<\/script>/gi, '<\\/script>');
  const modulePathsJson = JSON.stringify(modulePaths);
  const kidlispSourceEscaped = kidlispSource.replace(/\\/g, '\\\\').replace(/'/g, "\\'").replace(/\n/g, '\\n');

  const html = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>$${pieceName} - Aesthetic Computer Keep</title>
    <meta name="description" content="Aesthetic Computer KidLisp piece: $${pieceName}">
    <meta name="pack-date" content="${packDate}">
    <meta name="git-version" content="${gitVersion}">
    
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        html, body { width: 100%; height: 100%; overflow: hidden; background: #000; }
        canvas { display: block; width: 100%; height: 100%; }
        ${inlinedCSS}
    </style>
</head>
<body>
<script>
// Virtual File System with all embedded files
window.VFS = ${vfsJson};

// Embedded KidLisp source
window.EMBEDDED_KIDLISP_SOURCE = '${kidlispSourceEscaped}';
window.EMBEDDED_KIDLISP_PIECE = '${pieceName}';

console.log('📦 Aesthetic Computer Keep Bundle');
console.log('🎨 Piece: $${pieceName}');
console.log('📅 Packed: ${packDate}');
console.log('🔧 Git: ${gitVersion}');
console.log('📁 VFS Files: ${Object.keys(vfs).length}');
console.log('💾 KidLisp Source:', window.EMBEDDED_KIDLISP_SOURCE);
</script>

<script>
// Create blob URLs for all JS modules
window.VFS_BLOB_URLS = {};

console.log('🔧 Creating blob URLs for modules...');

window.modulePaths = ${modulePathsJson};

for (const filepath of window.modulePaths) {
  const filedata = window.VFS[filepath];
  if (filedata && !filedata.binary) {
    const rewrittenCode = rewriteImports(filedata.content, filepath);
    const blob = new Blob([rewrittenCode], { type: 'application/javascript' });
    const blobUrl = URL.createObjectURL(blob);
    window.VFS_BLOB_URLS[filepath] = blobUrl;
  }
}

console.log(\`✅ Created \${Object.keys(window.VFS_BLOB_URLS).length} blob URLs\`);

function rewriteImports(code, modulePath) {
  const dir = modulePath.split('/').slice(0, -1).join('/');
  
  code = code.replace(/from\\s+['"]aesthetic\\.computer\\/disks\\/([^'"]+)['"]/g, (match, path) => {
    return 'from \\'ac/disks/' + path + '\\'';
  });
  
  code = code.replace(/import\\s*\\((['"]aesthetic\\.computer\\/disks\\/([^'"]+)['")])\\)/g, (match, fullPath, path) => {
    return 'import(\\'ac/disks/' + path + '\\')';
  });
  
  code = code.replace(/from\\s+['"](\\.\\.\\/[^'"]+|\\.\\/[^'"]+)['"]/g, (match, path) => {
    const resolved = resolvePath(dir, path);
    return 'from \\'' + resolved + '\\'';
  });
  
  code = code.replace(/import\\s*\\((['"](\\.\\.\\/[^'"]+|\\.\\/[^'"]+)['")])\\)/g, (match, fullPath, path) => {
    const resolved = resolvePath(dir, path);
    return 'import(\\'' + resolved + '\\')';
  });
  
  code = code.replace(/import\\s*\\(\\\`(\\.\\.\\/[^\\\`]+|\\.\\/[^\\\`]+)\\\`\\)/g, (match, path) => {
    const resolved = resolvePath(dir, path);
    return 'import(\\'' + resolved + '\\')';
  });
  
  return code;
}

function resolvePath(base, relative) {
  if (!relative.startsWith('.')) return relative;
  
  const parts = base.split('/').filter(p => p);
  const relParts = relative.split('/');
  
  for (const part of relParts) {
    if (part === '..') parts.pop();
    else if (part !== '.' && part !== '') parts.push(part);
  }
  
  return parts.join('/');
}
</script>

<script>
// Create import map
const importMapEntries = {};

for (const filepath of window.modulePaths) {
  if (window.VFS_BLOB_URLS[filepath]) {
    importMapEntries[filepath] = window.VFS_BLOB_URLS[filepath];
    
    if (filepath.startsWith('ac/disks/')) {
      const legacyPath = filepath.replace('ac/disks/', 'aesthetic.computer/disks/');
      importMapEntries[legacyPath] = window.VFS_BLOB_URLS[filepath];
      importMapEntries['/' + legacyPath] = window.VFS_BLOB_URLS[filepath];
    }
    
    importMapEntries['/' + filepath] = window.VFS_BLOB_URLS[filepath];
  }
}

const importMapScript = document.createElement('script');
importMapScript.type = 'importmap';
importMapScript.textContent = JSON.stringify({ imports: importMapEntries });
document.head.appendChild(importMapScript);

console.log(\`✅ Import map: \${Object.keys(importMapEntries).length} entries\`);
</script>

<script>
// Intercept fetch() and XMLHttpRequest
const originalFetch = window.fetch;
const originalXHROpen = XMLHttpRequest.prototype.open;
const originalXHRSend = XMLHttpRequest.prototype.send;

window.fetch = function(url, options) {
  const urlStr = typeof url === 'string' ? url : url.url;
  
  // SPECIAL: Intercept KidLisp API calls and return embedded source
  if (urlStr.includes('/store-kidlisp') || urlStr.includes('/api/painting-code')) {
    console.log('🎯 Intercepted KidLisp API call, returning embedded source');
    return Promise.resolve(new Response(JSON.stringify({
      code: window.EMBEDDED_KIDLISP_SOURCE,
      name: window.EMBEDDED_KIDLISP_PIECE
    }), {
      status: 200,
      headers: { 'Content-Type': 'application/json' }
    }));
  }
  
  let vfsPath = urlStr
    .replace(/^\\.?\\//, '')
    .replace(/^https?:\\/\\/[^\\/]+\\//, '')
    .replace(/#.*$/, '')
    .replace(/\\?.*$/, '');
  
  try {
    vfsPath = decodeURIComponent(vfsPath);
  } catch (e) {}
  
  vfsPath = vfsPath.replace(/^aesthetic\\.computer\\//, '');
  
  if (!vfsPath.startsWith('ac/') && !vfsPath.startsWith('type/')) {
    vfsPath = vfsPath.replace(/^\\.\\.\\//g, '');
    if (!vfsPath.startsWith('ac/') && !vfsPath.startsWith('type/')) {
      vfsPath = 'ac/' + vfsPath;
    }
  }
  
  console.log('🔍 VFS fetch:', urlStr, '→', vfsPath);
  
  if (window.VFS[vfsPath]) {
    const file = window.VFS[vfsPath];
    let content = file.content;
    
    if (file.binary) {
      const binaryString = atob(content);
      const bytes = new Uint8Array(binaryString.length);
      for (let i = 0; i < binaryString.length; i++) {
        bytes[i] = binaryString.charCodeAt(i);
      }
      content = new Blob([bytes], { type: getMimeType(file.type) });
    }
    
    console.log('✓ VFS served:', vfsPath, file.binary ? '(binary)' : '(text)');
    
    return Promise.resolve(new Response(content, {
      status: 200,
      headers: { 'Content-Type': getMimeType(file.type) }
    }));
  }
  
  console.log('⚠️ VFS miss:', vfsPath, '- falling back to network');
  return originalFetch.apply(this, arguments);
};

// XHR intercept for font drawings
XMLHttpRequest.prototype.open = function(method, url, ...args) {
  this._url = url;
  return originalXHROpen.call(this, method, url, ...args);
};

XMLHttpRequest.prototype.send = function(...args) {
  const url = this._url;
  
  if (typeof url === 'string' && url.includes('aesthetic.computer/disks/drawings')) {
    let vfsPath = url
      .replace(/^https?:\\/\\/[^\\/]+\\//, '')
      .replace(/^aesthetic\\.computer\\//, 'ac/');
    
    try {
      vfsPath = decodeURIComponent(vfsPath);
    } catch (e) {}
    
    console.log('🔍 XHR fetch:', url, '→', vfsPath);
    
    if (window.VFS[vfsPath]) {
      const file = window.VFS[vfsPath];
      
      Object.defineProperty(this, 'responseText', { value: file.content, writable: false });
      Object.defineProperty(this, 'response', { value: file.content, writable: false });
      Object.defineProperty(this, 'status', { value: 200, writable: false });
      Object.defineProperty(this, 'readyState', { value: 4, writable: false });
      
      console.log('✓ XHR served from VFS:', vfsPath);
      
      setTimeout(() => {
        if (this.onreadystatechange) this.onreadystatechange();
        if (this.onload) this.onload();
      }, 0);
      
      return;
    }
  }
  
  return originalXHRSend.apply(this, args);
};

function getMimeType(ext) {
  const mimeTypes = {
    'js': 'application/javascript',
    'mjs': 'application/javascript',
    'json': 'application/json',
    'css': 'text/css',
    'html': 'text/html',
    'svg': 'image/svg+xml',
    'woff': 'font/woff',
    'woff2': 'font/woff2',
    'ttf': 'font/ttf',
    'otf': 'font/otf'
  };
  return mimeTypes[ext] || 'text/plain';
}
</script>

<script>
// Load style.css from VFS
if (window.VFS['ac/style.css']) {
  const styleEl = document.createElement('style');
  styleEl.textContent = window.VFS['ac/style.css'].content;
  document.head.appendChild(styleEl);
  console.log('✅ Loaded style.css from VFS');
}

// Boot aesthetic.computer with embedded KidLisp
const bootUrl = window.VFS_BLOB_URLS['ac/boot.mjs'];
if (bootUrl) {
  console.log('🚀 Booting aesthetic.computer...');
  import(bootUrl).catch(err => console.error('Boot error:', err));
} else {
  console.error('❌ boot.mjs not found in VFS');
}
</script>

</body>
</html>`;

  // Write output
  await fs.mkdir(OUTPUT_DIR, { recursive: true });
  const outputPath = path.join(OUTPUT_DIR, `${OUTPUT_NAME}-keep.html`);
  await fs.writeFile(outputPath, html);

  const stats = await fs.stat(outputPath);
  const sizeMB = (stats.size / 1024 / 1024).toFixed(2);

  console.log(`\n✅ Keep bundle created!`);
  console.log(`   📄 ${outputPath}`);
  console.log(`   💾 Size: ${sizeMB} MB`);
  console.log(`   🎨 Piece: $${pieceName}`);
  console.log(`   📝 KidLisp: ${kidlispSource.length} chars`);
  console.log(`\n🚀 This file is completely self-contained!`);
  console.log(`   • Embedded KidLisp source`);
  console.log(`   • No external API calls`);
  console.log(`   • No IPFS/CDN required`);
  console.log(`   • Ready for Tezos minting`);
  console.log(`   • Timestamp: ${packDate}`);

  return outputPath;
}

// Main
(async () => {
  try {
    const kidlispSource = await fetchKidLispSource(PIECE_NAME);
    const outputPath = await createKeepBundle(PIECE_NAME, kidlispSource);
    
    console.log(`\n📦 To test locally:`);
    console.log(`   cd ${OUTPUT_DIR}`);
    console.log(`   python3 -m http.server 8891`);
    console.log(`   Open: http://localhost:8891/${OUTPUT_NAME}-keep.html`);
    
  } catch (error) {
    console.error(`\n❌ Error: ${error.message}`);
    console.error(error.stack);
    process.exit(1);
  }
})();
