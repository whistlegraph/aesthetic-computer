#!/usr/bin/env node

// Tezos Keep Bundler - creates one self-contained HTML file for Tezos NFTs
// Excludes image assets and generates directly from source
// For KidLisp pieces, fetches and embeds source code at build time
// Usage: node bundle-kidlisp-keep.mjs <piece-name>

import { promises as fs } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { execSync } from "child_process";
import https from "https";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const PIECE_NAME = process.argv[2] || "$ceo";
const OUTPUT_DIR = path.join(__dirname, "keep-bundles");
const SOURCE_DIR = path.resolve(__dirname, "..");

// Get git info for timestamp
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
            console.log(`✅ KidLisp source: ${json.source.substring(0, 80)}...`);
            resolve(json.source);
          } else {
            console.log(`⚠️ No KidLisp source found for ${piece}`);
            resolve(null);
          }
        } catch (e) {
          console.log(`⚠️ Not a KidLisp piece: ${piece}`);
          resolve(null);
        }
      });
    }).on('error', (e) => {
      console.log(`⚠️ Could not fetch KidLisp source: ${e.message}`);
      resolve(null);
    });
  });
}

// Recursively fetch all embedded KidLisp pieces
async function fetchAllEmbeddedPieces(mainPiece) {
  const pieces = new Map(); // Map of piece name -> source
  const toFetch = [mainPiece];
  const fetched = new Set();

  console.log(`\n🔍 Scanning for embedded KidLisp pieces...`);

  while (toFetch.length > 0) {
    const piece = toFetch.shift();
    if (fetched.has(piece)) continue;
    
    fetched.add(piece);
    const source = await fetchKidLispSource(piece);
    
    if (source) {
      pieces.set(piece, source);
      
      // Scan for embedded $codes in the source
      const embeddedMatches = source.matchAll(/\$([a-z0-9]+)/gi);
      for (const match of embeddedMatches) {
        const embeddedPiece = match[1];
        if (!fetched.has(embeddedPiece) && !toFetch.includes(embeddedPiece)) {
          console.log(`   📎 Found embedded piece: $${embeddedPiece}`);
          toFetch.push(embeddedPiece);
        }
      }
    }
  }

  console.log(`✅ Fetched ${pieces.size} total piece(s): ${Array.from(pieces.keys()).map(p => '$' + p).join(', ')}`);
  return pieces;
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
      // Determine if binary
      const ext = path.extname(entry.name).toLowerCase();
      const isBinary = [".png", ".gif", ".jpg", ".jpeg", ".ico", ".woff", ".woff2", ".ttf", ".otf"].includes(ext);
      const isImage = [".png", ".gif", ".jpg", ".jpeg", ".ico"].includes(ext);
      
      // Skip images if excludeImages is true
      if (excludeImages && isImage) {
        continue;
      }
      
      files[relativePath] = {
        content: await inlineFile(fullPath, isBinary ? "binary" : "text"),
        binary: isBinary,
        type: ext.slice(1) || "txt"
      };
    }
  }

  return files;
}

async function createSingleFile(kidlispSource, allPieces = new Map()) {
  console.log(`\n📦 Creating single-file HTML for ${PIECE_NAME}...`);
  console.log(`📅 Pack date: ${packDate}`);
  console.log(`🔧 Git version: ${gitVersion}`);

  // Inline essential files from source ac/ directory (excluding images and heavy deps)
  console.log("\n📁 Inlining ac/ directory...");
  const acDir = path.join(SOURCE_DIR, "system/public/aesthetic.computer");
  const essentialDirs = ["boot.mjs", "bios.mjs", "lib", "disks", "systems", "style.css", "cursors"];
  
  const acFiles = {};
  for (const item of essentialDirs) {
    const itemPath = path.join(acDir, item);
    const stats = await fs.stat(itemPath).catch(() => null);
    
    if (!stats) continue;
    
    if (stats.isDirectory()) {
      const dirFiles = await inlineDirectory(itemPath, acDir, false); // Include cursors (SVG files)
      Object.assign(acFiles, dirFiles);
    } else {
      // Single file
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
  
  // Add only essential dependencies from dep/
  console.log("📁 Inlining dep/gl-matrix...");
  const glMatrixDir = path.join(acDir, "dep/gl-matrix");
  const glMatrixFiles = await inlineDirectory(glMatrixDir, acDir, true);
  Object.assign(acFiles, glMatrixFiles);

  console.log("📁 Inlining dep/@akamfoad/qr...");
  const qrDir = path.join(acDir, "dep/@akamfoad/qr");
  const qrFiles = await inlineDirectory(qrDir, acDir, true);
  Object.assign(acFiles, qrFiles);

  console.log("📁 Inlining dep/idb.js...");
  const idbPath = path.join(acDir, "dep/idb.js");
  const idbContent = await fs.readFile(idbPath, "utf8");
  acFiles["dep/idb.js"] = {
    content: idbContent,
    binary: false,
    type: "js"
  };

  console.log("📁 Inlining dep/nanoid...");
  const nanoidDir = path.join(acDir, "dep/nanoid");
  const nanoidFiles = await inlineDirectory(nanoidDir, acDir, true);
  Object.assign(acFiles, nanoidFiles);

  console.log("📁 Inlining dep/geckos.io-client...");
  const geckosPath = path.join(acDir, "dep/geckos.io-client.2.3.2.min.js");
  const geckosContent = await fs.readFile(geckosPath, "utf8");
  acFiles["dep/geckos.io-client.2.3.2.min.js"] = {
    content: geckosContent,
    binary: false,
    type: "js"
  };
  
  console.log(`   ✓ Found ${Object.keys(acFiles).length} files`);

  // Inline type/ directory (fonts)
  console.log("📁 Inlining type/ directory...");
  const typeDir = path.join(SOURCE_DIR, "system/public/type");
  const typeFiles = await inlineDirectory(typeDir, typeDir, false);
  console.log(`   ✓ Found ${Object.keys(typeFiles).length} files`);

  // Create virtual filesystem (no images!)
  const vfs = {
    ...Object.fromEntries(Object.entries(acFiles).map(([k, v]) => [`ac/${k}`, v])),
    ...Object.fromEntries(Object.entries(typeFiles).map(([k, v]) => [`type/${k}`, v]))
  };

  console.log(`\n📊 Total files in VFS: ${Object.keys(vfs).length}`);

  // Prepare KidLisp embedding
  const kidlispSourceEscaped = kidlispSource 
    ? kidlispSource.replace(/\\/g, '\\\\').replace(/'/g, "\\'").replace(/\n/g, '\\n')
    : '';
  
  if (kidlispSource) {
    console.log(`📝 Embedded KidLisp source: ${kidlispSource.length} chars`);
  }

  // Create the single file HTML
  // CRITICAL: Escape </script> in JSON to prevent premature script tag closure
  const vfsJson = JSON.stringify(vfs).replace(/<\/script>/gi, '<\\/script>');
  
  // Build list of all module paths for runtime import map generation
  const modulePaths = Object.keys(vfs).filter(path => 
    path.endsWith('.js') || path.endsWith('.mjs')
  );
  
  console.log(`📦 Found ${modulePaths.length} JavaScript modules for import map`);
  
  const modulePathsJson = JSON.stringify(modulePaths);
  
  const singleFileHtml = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>${PIECE_NAME} - Aesthetic Computer</title>

<script>
console.log('Aesthetic.Computer');
console.log('');
console.log('${PIECE_NAME} is a piece by @jeffrey');
console.log('');
${kidlispSource ? `console.log('KidLisp source: ${kidlispSourceEscaped}');` : `console.log('(source embedded in hash)');`}
console.log('');
console.log('This copy was packed on ${packDate}');
console.log('');
console.log('Using aesthetic-computer git version ${gitVersion}');

// Virtual FileSystem - all files embedded as base64/text
window.VFS = ${vfsJson};

${kidlispSource ? `// Embedded KidLisp source for OBJKT offline mode
window.objktKidlispCodes = ${JSON.stringify(Object.fromEntries(
  Array.from(allPieces.entries()).map(([name, source]) => [
    name,
    source
  ])
))};
window.EMBEDDED_KIDLISP_SOURCE = '${kidlispSourceEscaped}';
window.EMBEDDED_KIDLISP_PIECE = '${PIECE_NAME.replace(/^\$/, '')}';
console.log('📝 Embedded ${allPieces.size} KidLisp piece(s) for offline use');
` : ''}
window.acPACK_MODE = true;
window.acSTARTING_PIECE = '${PIECE_NAME}';
window.acPACK_PIECE = '${PIECE_NAME}';
window.acPACK_DATE = '${packDate}';
window.acPACK_VERSION = '${gitVersion}';

// Intercept appendChild to prevent CSS link elements from being added
const originalAppendChild = Element.prototype.appendChild;
Element.prototype.appendChild = function(child) {
  // Block CSS link elements to prevent 404s (fonts already work via VFS XHR)
  if (child.tagName === 'LINK' && child.rel === 'stylesheet' && child.href && child.href.includes('.css')) {
    console.log('🚫 Blocked CSS link to prevent 404:', child.href);
    return child; // Return without appending
  }
  return originalAppendChild.call(this, child);
};
console.log('✅ CSS link blocking installed');

// Also intercept document.body.append (used in bios.mjs)
const originalBodyAppend = HTMLBodyElement.prototype.append;
HTMLBodyElement.prototype.append = function(...nodes) {
  const filteredNodes = nodes.filter(node => {
    if (node.tagName === 'LINK' && node.rel === 'stylesheet' && node.href && node.href.includes('.css')) {
      console.log('🚫 Blocked CSS link via body.append:', node.href);
      return false;
    }
    return true;
  });
  return originalBodyAppend.call(this, ...filteredNodes);
};
console.log('✅ CSS link blocking (body.append) installed');

// Intercept Image src loading to serve SVG cursors from VFS
const OriginalImage = window.Image;
window.Image = function(...args) {
  const img = new OriginalImage(...args);
  const originalSrcDescriptor = Object.getOwnPropertyDescriptor(HTMLImageElement.prototype, 'src');
  
  Object.defineProperty(img, 'src', {
    set: function(value) {
      // Intercept cursor SVG loading
      if (value && value.includes('.svg') && (value.includes('cursors/') || value.includes('aesthetic.computer'))) {
        console.log('🖼️ Intercepting Image src:', value);
        
        // Normalize path
        let vfsPath = value
          .replace(/^\\.?\\//, '')
          .replace(/^https?:\\/\\/[^\\/]+\\//, '')
          .replace(/^aesthetic\\.computer\\//, '');
        
        // Add ac/ prefix if not present
        if (!vfsPath.startsWith('ac/') && !vfsPath.startsWith('type/')) {
          vfsPath = 'ac/' + vfsPath;
        }
        
        console.log('🖼️ Looking for VFS path:', vfsPath);
        
        if (window.VFS[vfsPath]) {
          const file = window.VFS[vfsPath];
          // Create data URI for SVG
          const svgContent = file.binary ? atob(file.content) : file.content;
          const dataUri = 'data:image/svg+xml;base64,' + (file.binary ? file.content : btoa(svgContent));
          console.log('✓ Image served from VFS:', vfsPath);
          originalSrcDescriptor.set.call(this, dataUri);
          return;
        }
      }
      // Fallback to original behavior
      originalSrcDescriptor.set.call(this, value);
    },
    get: function() {
      return originalSrcDescriptor.get.call(this);
    }
  });
  
  return img;
};
console.log('✅ Image src interception installed');

// Create blob URLs for all JS modules with rewritten imports
window.VFS_BLOB_URLS = {};

console.log('🔧 Creating blob URLs for all modules...');

// Paths of all modules that need blob URLs
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

// Helper to rewrite imports in module code
function rewriteImports(code, modulePath) {
  const dir = modulePath.split('/').slice(0, -1).join('/');
  
  // Rewrite aesthetic.computer/ imports to ac/ format
  code = code.replace(/from\\s+['"]aesthetic\\.computer\\/disks\\/([^'"]+)['"]/g, (match, path) => {
    return 'from \\'ac/disks/' + path + '\\'';
  });
  
  code = code.replace(/import\\s*\\((['"]aesthetic\\.computer\\/disks\\/([^'"]+)['")])\\)/g, (match, fullPath, path) => {
    return 'import(\\'ac/disks/' + path + '\\')';
  });
  
  // Rewrite relative imports to bare specifiers (no leading /)
  code = code.replace(/from\\s+['"](\\.\\.\\/[^'"]+|\\.\\/[^'"]+)['"]/g, (match, path) => {
    const resolved = resolvePath(dir, path);
    return 'from \\'' + resolved + '\\''; // No leading /
  });
  
  code = code.replace(/import\\s*\\((['"](\\.\\.\\/[^'"]+|\\.\\/[^'"]+)['")])\\)/g, (match, fullPath, path) => {
    const resolved = resolvePath(dir, path);
    return 'import(\\'' + resolved + '\\')'; // No leading /
  });
  
  // Also handle backtick template literals: import(\\\`./path\\\`)
  code = code.replace(/import\\s*\\(\\\`(\\.\\.\\/[^\\\`]+|\\.\\/[^\\\`]+)\\\`\\)/g, (match, path) => {
    const resolved = resolvePath(dir, path);
    return 'import(\\'' + resolved + '\\')'; // Convert to regular quotes with resolved path
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
const importMapEntries = {};

for (const filepath of window.modulePaths) {
  if (window.VFS_BLOB_URLS[filepath]) {
    // Add normalized path (ac/disks/...)
    importMapEntries[filepath] = window.VFS_BLOB_URLS[filepath];
    
    // Also add legacy path (aesthetic.computer/disks/...)
    if (filepath.startsWith('ac/disks/')) {
      const legacyPath = filepath.replace('ac/disks/', 'aesthetic.computer/disks/');
      importMapEntries[legacyPath] = window.VFS_BLOB_URLS[filepath];
      // Also add with leading slash
      importMapEntries['/' + legacyPath] = window.VFS_BLOB_URLS[filepath];
    }
    
    // Also add with leading slash for ac/ paths
    importMapEntries['/' + filepath] = window.VFS_BLOB_URLS[filepath];
  }
}

const importMapScript = document.createElement('script');
importMapScript.type = 'importmap';
importMapScript.textContent = JSON.stringify({ imports: importMapEntries });
document.head.appendChild(importMapScript);

console.log(\`✅ Import map activated with \${Object.keys(importMapEntries).length} entries\`);
console.log(\`📝 Sample entries:\`, Object.keys(importMapEntries).filter(k => k.includes('sfx')).slice(0, 3));
</script>

<script>
// Intercept fetch() and XMLHttpRequest to serve from VFS
const originalFetch = window.fetch;
const originalXHROpen = XMLHttpRequest.prototype.open;
const originalXHRSend = XMLHttpRequest.prototype.send;

window.fetch = function(url, options) {
  const urlStr = typeof url === 'string' ? url : url.url;
  
  // SPECIAL: Intercept KidLisp/painting API calls and return embedded data
  if (window.EMBEDDED_KIDLISP_SOURCE) {
    // Intercept painting-code lookups for the embedded piece
    if (urlStr.includes('/api/painting-code') && urlStr.includes(window.EMBEDDED_KIDLISP_PIECE)) {
      console.log('🎯 Intercepted painting-code API, returning embedded KidLisp metadata');
      return Promise.resolve(new Response(JSON.stringify({
        slug: window.EMBEDDED_KIDLISP_PIECE,
        handle: 'jeffrey',
        code: window.EMBEDDED_KIDLISP_PIECE
      }), {
        status: 200,
        headers: { 'Content-Type': 'application/json' }
      }));
    }
    
    // Intercept KidLisp source fetches
    if (urlStr.includes('/store-kidlisp') || urlStr.includes(\`/\${window.EMBEDDED_KIDLISP_PIECE}.lisp\`)) {
      console.log('🎯 Intercepted KidLisp source API, returning embedded source');
      return Promise.resolve(new Response(window.EMBEDDED_KIDLISP_SOURCE, {
        status: 200,
        headers: { 'Content-Type': 'text/plain' }
      }));
    }
  }
  
  // Normalize URL (remove leading ./ and absolute paths, and strip hash/query)
  let vfsPath = urlStr
    .replace(/^\\.?\\//, '')
    .replace(/^https?:\\/\\/[^\\/]+\\//, '')
    .replace(/#.*$/, '')  // Strip hash (timestamp)
    .replace(/\\?.*$/, ''); // Strip query params
  
  // Decode URL encoding (%20 -> space, etc.)
  try {
    vfsPath = decodeURIComponent(vfsPath);
  } catch (e) {
    console.warn('Failed to decode URL:', vfsPath);
  }
  
  // Remove 'aesthetic.computer/' prefix if present (legacy paths)
  vfsPath = vfsPath.replace(/^aesthetic\\.computer\\//, '');
  
  // Add ac/ prefix if not present (for relative paths like ../disks/prompt.mjs or cursors/precise.svg)
  if (!vfsPath.startsWith('ac/') && !vfsPath.startsWith('type/')) {
    // Remove leading ../ if present
    vfsPath = vfsPath.replace(/^\\.\\.\\//g, '');
    if (!vfsPath.startsWith('ac/') && !vfsPath.startsWith('type/')) {
      vfsPath = 'ac/' + vfsPath;
    }
  }
  
  console.log('🔍 VFS fetch:', urlStr, '→', vfsPath);
  
  if (window.VFS[vfsPath]) {
    const file = window.VFS[vfsPath];
    let content = file.content;
    
    // Convert base64 to blob for binary files
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
      headers: {
        'Content-Type': file.binary ? getMimeType(file.type) : 'text/plain'
      }
    }));
  }
  
  console.warn('⚠️ VFS miss:', vfsPath, '- falling back to network');
  return originalFetch(url, options);
};

// Intercept XMLHttpRequest to serve from VFS
XMLHttpRequest.prototype.open = function(method, url, ...args) {
  this._url = url; // Store for later
  return originalXHROpen.call(this, method, url, ...args);
};

XMLHttpRequest.prototype.send = function(...args) {
  if (this._url) {
    const urlStr = this._url;
    
    // Same normalization as fetch
    let vfsPath = urlStr
      .replace(/^\\.?\\//, '')
      .replace(/^https?:\\/\\/[^\\/]+\\//, '')
      .replace(/#.*$/, '')
      .replace(/\\?.*$/, '');
    
    // Decode URL encoding
    try {
      vfsPath = decodeURIComponent(vfsPath);
    } catch (e) {
      console.warn('Failed to decode URL:', vfsPath);
    }
    
    vfsPath = vfsPath.replace(/^aesthetic\\.computer\\//, '');
    
    if (!vfsPath.startsWith('ac/') && !vfsPath.startsWith('type/')) {
      vfsPath = vfsPath.replace(/^\\.\\.\\//g, '');
      if (!vfsPath.startsWith('ac/') && !vfsPath.startsWith('type/')) {
        vfsPath = 'ac/' + vfsPath;
      }
    }
    
    console.log('🔍 XHR fetch:', urlStr, '→', vfsPath);
    
    if (window.VFS[vfsPath]) {
      const file = window.VFS[vfsPath];
      
      // Override XHR to return VFS content
      Object.defineProperty(this, 'responseText', { 
        writable: true, 
        value: file.content 
      });
      Object.defineProperty(this, 'response', { 
        writable: true, 
        value: file.content 
      });
      Object.defineProperty(this, 'status', { 
        writable: true, 
        value: 200 
      });
      Object.defineProperty(this, 'readyState', { 
        writable: true, 
        value: 4 
      });
      
      console.log('✓ XHR served from VFS:', vfsPath);
      
      // Trigger events
      setTimeout(() => {
        if (this.onreadystatechange) this.onreadystatechange();
        if (this.onload) this.onload();
      }, 0);
      
      return;
    }
    
    console.warn('⚠️ XHR VFS miss:', vfsPath, '- falling back to network');
  }
  
  return originalXHRSend.call(this, ...args);
};

// Get MIME type from extension
function getMimeType(ext) {
  const types = {
    'js': 'application/javascript',
    'mjs': 'application/javascript',
    'json': 'application/json',
    'css': 'text/css',
    'html': 'text/html',
    'png': 'image/png',
    'gif': 'image/gif',
    'jpg': 'image/jpeg',
    'jpeg': 'image/jpeg',
    'svg': 'image/svg+xml',
    'ico': 'image/x-icon',
    'woff': 'font/woff',
    'woff2': 'font/woff2',
    'ttf': 'font/ttf',
    'otf': 'font/otf'
  };
  return types[ext] || 'application/octet-stream';
}
</script>

<script type="module">
// Load style.css from VFS
const styleEl = document.createElement('style');
styleEl.textContent = window.VFS['ac/style.css'].content;
document.head.appendChild(styleEl);

// Auto-load the piece if no hash is set
if (!window.location.hash) {
  window.location.hash = '#${PIECE_NAME}';
}

// Load boot.mjs using blob URL
const bootUrl = window.VFS_BLOB_URLS['ac/boot.mjs'] || window.VFS_BLOB_URLS['boot.mjs'];
if (!bootUrl) {
  console.error('Failed to find blob URL for boot.mjs');
  console.log('Available VFS_BLOB_URLS keys:', Object.keys(window.VFS_BLOB_URLS).filter(k => k.includes('boot')));
  document.body.innerHTML = '<pre>Error: Could not load boot.mjs from VFS</pre>';
} else {
  console.log('🚀 Loading boot from:', bootUrl);
  import(bootUrl).catch(err => {
    console.error('Failed to load boot:', err);
    document.body.innerHTML = '<pre>Error loading boot.mjs: ' + err.message + '\\n\\n' + err.stack + '</pre>';
  });
}
</script>

</head>
<body class="nogap native-cursor">
<canvas id="canvas"></canvas>
<div id="console"></div>
</body>
</html>`;

  // Write single file
  const outputPath = path.join(OUTPUT_DIR, `${PIECE_NAME.replace(/\$/g, '')}-nft.html`);
  await fs.writeFile(outputPath, singleFileHtml);

  const stats = await fs.stat(outputPath);
  const sizeMB = (stats.size / 1024 / 1024).toFixed(2);

  console.log(`\n✅ Keep bundle created!`);
  console.log(`   📄 ${outputPath}`);
  console.log(`   💾 Size: ${sizeMB} MB`);
  console.log(`   🖼️  No images included (code only)`);
  console.log(`\n🚀 This file is completely self-contained!`);
  console.log(`   • No external dependencies`);
  console.log(`   • No IPFS/CDN required`);
  console.log(`   • Ready for Tezos minting`);
  console.log(`   • Can be minted directly`);
  if (kidlispSource) {
    console.log(`   • KidLisp source embedded (${kidlispSource.length} chars)`);
  }
  console.log(`   • Current timestamp: ${packDate}`);
}

// Main - fetch all KidLisp sources (including embedded pieces), then create bundle
(async () => {
  try {
    const allPieces = await fetchAllEmbeddedPieces(PIECE_NAME.replace(/^\$/, ''));
    const mainSource = allPieces.get(PIECE_NAME.replace(/^\$/, ''));
    await createSingleFile(mainSource, allPieces);
  } catch (error) {
    console.error("❌ Error:", error);
    process.exit(1);
  }
})();
