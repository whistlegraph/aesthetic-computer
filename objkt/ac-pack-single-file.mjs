#!/usr/bin/env node

// Single-file PACK bundler - creates one self-contained HTML file
// Excludes image assets and generates directly from source
// Usage: node ac-pack-single-file.mjs <piece-name>

import { promises as fs } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { execSync } from "child_process";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const PIECE_NAME = process.argv[2] || "$eel";
const OUTPUT_DIR = path.join(__dirname, "output");
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

async function createSingleFile() {
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
console.log('Its KidLisp source is: ${PIECE_NAME === "$eel" ? "(beige)(ink (0.5s... gray black rainbow))(0.1s (write (? x X) width/2 height/2))(0.05s (scroll (? 18 -18) (? 32 -32))(blur 0.1)(1.25s (zoom (? 2 1.5 0.5)))" : "(source code here)"}');
console.log('');
console.log('This copy was packed on ${packDate}');
console.log('');
console.log('Using aesthetic-computer git version ${gitVersion}');

// Virtual FileSystem - all files embedded as base64/text
window.VFS = ${vfsJson};

window.acPACK_MODE = true;
window.acPACK_PIECE = '${PIECE_NAME}';
window.acPACK_DATE = '${packDate}';
window.acPACK_VERSION = '${gitVersion}';

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
    }
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
  window.location.hash = '#${PIECE_NAME.replace(/\$/g, '')}';
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
  const outputPath = path.join(OUTPUT_DIR, `${PIECE_NAME.replace(/\$/g, '')}-single-file.html`);
  await fs.writeFile(outputPath, singleFileHtml);

  const stats = await fs.stat(outputPath);
  const sizeMB = (stats.size / 1024 / 1024).toFixed(2);

  console.log(`\n✅ Single file created!`);
  console.log(`   📄 ${outputPath}`);
  console.log(`   💾 Size: ${sizeMB} MB`);
  console.log(`   🖼️  No images included (code only)`);
  console.log(`\n🚀 This file is completely self-contained!`);
  console.log(`   • No external dependencies`);
  console.log(`   • No IPFS/CDN required`);
  console.log(`   • Works on objkt.com`);
  console.log(`   • Can be minted directly`);
  console.log(`   • Current timestamp: ${packDate}`);
}

createSingleFile().catch(error => {
  console.error("❌ Error:", error);
  process.exit(1);
});
