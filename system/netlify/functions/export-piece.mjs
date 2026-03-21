// Export Piece, 25.11.21 [DEPRECATED — bundling now happens on oven]
// Generate self-contained HTML bundles for aesthetic.computer pieces
// Supports both KidLisp pieces ($pie) and regular .mjs pieces (line)
// Usage: GET /.netlify/functions/export-piece?piece=$pie  or  ?piece=line

import { minify } from "terser";
import { respond } from "../../backend/http.mjs";
import { readFile, readdir } from "fs/promises";
import { resolve, join, dirname, basename } from "path";
import { fileURLToPath } from "url";
import zlib from "zlib";
import { promisify } from "util";
import { execSync } from "child_process";
import { font_1, microtype } from "../../public/aesthetic.computer/disks/common/fonts.mjs";

const gzip = promisify(zlib.gzip);

// Resolve public directory
// Netlify functions might not have import.meta.url, so use __dirname fallback
let publicDir, fontDir;
try {
  if (import.meta.url) {
    const __dirname = dirname(fileURLToPath(import.meta.url));
    publicDir = resolve(__dirname, "../../public/aesthetic.computer");
    fontDir = resolve(publicDir, "../type/webfonts");
  } else {
    throw new Error("import.meta.url not available");
  }
} catch (e) {
  // Fallback: Netlify functions run from system/ directory
  publicDir = resolve(process.cwd(), "public/aesthetic.computer");
  fontDir = resolve(publicDir, "../type/webfonts");
}

// Minimal set of files needed for basic KidLisp pieces
const ESSENTIAL_FILES = [
  // Core system
  'boot.mjs',
  'bios.mjs',
  
  // Essential libraries
  'lib/loop.mjs',
  'lib/disk.mjs',
  'lib/kidlisp.mjs',
  'lib/graph.mjs',
  'lib/geo.mjs',
  'lib/2d.mjs',
  'lib/3d.mjs',
  'lib/pen.mjs',
  'lib/parse.mjs',
  'lib/num.mjs',
  'lib/help.mjs',
  'lib/helpers.mjs',
  'lib/type.mjs',
  'lib/ui.mjs',
  'lib/keyboard.mjs',
  'lib/platform.mjs',
  'lib/gamepad.mjs',
  'lib/gamepad-mappings.mjs',
  'lib/gesture.mjs',
  'lib/gl.mjs',
  'lib/glaze.mjs',
  'lib/glazes/uniforms.js',
  'lib/speaker.mjs',
  'lib/sound/synth.mjs',
  'lib/sound/volume.mjs',
  'lib/sound/bubble.mjs',
  'lib/sound/sound-whitelist.mjs',
  'lib/text.mjs',
  'lib/headers.mjs',
  'lib/color-highlighting.mjs',
  'lib/cam-doll.mjs',
  'lib/ticker.mjs',
  'lib/gizmo.mjs',
  'lib/hand.mjs',
  'lib/hand-processor.js',
  'lib/motion.mjs',
  'lib/microphone.mjs',
  'lib/midi.mjs',
  'lib/melody-parser.mjs',
  'lib/notepat-convert.mjs',
  'lib/usb.mjs',
  'lib/chat.mjs',
  'lib/socket.mjs',
  'lib/udp.mjs',
  'lib/logs.mjs',
  'lib/redact.mjs',
  'lib/ask.mjs',
  'lib/speech.mjs',
  'lib/store.mjs',
  'lib/shop.mjs',
  'lib/user-code.mjs',
  'lib/pack-mode.mjs',
  'lib/fade-state.mjs',
  'lib/graphics-optimizer.mjs',
  'lib/ticket.mjs',
  
  // Systems
  'systems/nopaint.mjs',
  'systems/prompt-system.mjs',
  'systems/world.mjs',
  
  // Common disk files
  'disks/common/fonts.mjs',
  'disks/common/debug.mjs',
  'disks/common/music.mjs',
  'disks/common/products.mjs',
  'disks/common/scrub.mjs',
  'disks/common/sfx.mjs',
  'disks/common/tape-player.mjs',
];

const ESSENTIAL_DEPS = [
  'dep/gl-matrix/common.mjs',
  'dep/gl-matrix/index.mjs',
  'dep/gl-matrix/vec2.mjs',
  'dep/gl-matrix/vec3.mjs',
  'dep/gl-matrix/vec4.mjs',
  'dep/gl-matrix/mat2.mjs',
  'dep/gl-matrix/mat2d.mjs',
  'dep/gl-matrix/mat3.mjs',
  'dep/gl-matrix/mat4.mjs',
  'dep/gl-matrix/quat.mjs',
  'dep/gl-matrix/quat2.mjs',
  'dep/@akamfoad/qr/qr.mjs',
  'dep/idb.js',
  'dep/nanoid/nanoid.js',
  'dep/nanoid/url-alphabet/index.js',
  'dep/geckos.io-client.2.3.2.min.js',
];

// HTTP client for API calls (handles self-signed certs in dev)
async function getHttpClient() {
  const got = (await import("got")).default;
  return got.extend({
    https: { rejectUnauthorized: process.env.NETLIFY_DEV !== "true" }
  });
}

async function fetchKidLispSource(piece) {
  try {
    // Always fetch from production to get real KidLisp code
    const url = `https://aesthetic.computer/.netlify/functions/store-kidlisp?code=${piece}`;
    
    console.log(`📡 Fetching KidLisp source from production: ${url}`);
    const httpClient = await getHttpClient();
    const response = await httpClient.get(url).json();
    console.log(`📡 Retrieved KidLisp source for ${piece}: ${response.source?.length || 0} chars`);
    return response.source || null;
  } catch (e) {
    console.error(`📡 Error fetching KidLisp source for ${piece}:`, e.message);
    
    // Fallback: provide a simple test piece if fetch fails
    if (piece === 'pie') {
      console.log(`📡 Using fallback test code for ${piece}`);
      return '(wipe 200 100 255)\n(ink 255 200 0)\n(box 100 100 50)';
    }
  }
  return null;
}

async function fetchMjsPieceSource(piece) {
  try {
    const url = `https://aesthetic.computer/disks/${piece}.mjs`;
    console.log(`📡 Fetching .mjs piece from production: ${url}`);
    const httpClient = await getHttpClient();
    const response = await httpClient.get(url).text();
    console.log(`📡 Retrieved .mjs source for ${piece}: ${response.length} chars`);
    return response;
  } catch (e) {
    console.error(`📡 Error fetching .mjs piece ${piece}:`, e.message);
    return null;
  }
}

// Read file from local filesystem
async function readLocalFile(relativePath) {
  try {
    const filePath = resolve(publicDir, relativePath);
    return await readFile(filePath, "utf-8");
  } catch (e) {
    console.warn(`⚠️ Could not read ${relativePath}:`, e.message);
    return null;
  }
}

async function getInlinedCSS() {
  const cssFiles = [
    "berkeley-mono-variable.css",
    "ywft-processing-regular.css",
    "ywft-processing-light.css",
    "ywft-processing-bold.css",
  ];

  let combinedCSS = "";

  for (const cssFile of cssFiles) {
    try {
      const cssPath = resolve(fontDir, cssFile);
      let cssContent = await readFile(cssPath, "utf-8");

      // Find all url(...) references — only inline .woff2 files
      const urlRegex = /url\(['"]?([^'"\)]+)['"]?\)/g;
      const replacements = [];
      let match;

      while ((match = urlRegex.exec(cssContent)) !== null) {
        const relativeUrl = match[1];
        if (relativeUrl.startsWith("data:") || relativeUrl.startsWith("#")) continue;
        const cleanUrl = relativeUrl.split("?")[0].split("#")[0];
        // Only inline woff2 — skip eot, woff, ttf, svg
        if (!cleanUrl.endsWith(".woff2")) continue;
        const fontPath = resolve(fontDir, cleanUrl);
        try {
          const fontData = await readFile(fontPath);
          const base64 = fontData.toString("base64");
          replacements.push({
            original: match[0],
            replacement: `url('data:font/woff2;base64,${base64}')`
          });
        } catch (err) {
          // Skip missing font files
        }
      }

      for (const { original, replacement } of replacements) {
        cssContent = cssContent.replace(original, replacement);
      }

      // Strip non-woff2 src entries: rewrite @font-face to only keep woff2
      // Remove standalone eot fallback lines: src: url('x.eot');
      cssContent = cssContent.replace(/\bsrc:\s*url\([^)]*\.eot[^)]*\)\s*;\s*\n?/g, '');
      // From multi-format src blocks, keep only the woff2 (data: URI) entry
      cssContent = cssContent.replace(
        /\bsrc:\s*(.*?)\s*;/gs,
        (match, srcBody) => {
          // Find the woff2 entry (now a data: URI)
          const woff2Match = srcBody.match(/url\('data:font\/woff2;base64,[^']+'\)\s*format\(['"]woff2['"]\)/);
          if (woff2Match) return `src: ${woff2Match[0]};`;
          // For Berkeley Mono (no format()), keep data: URI entries as-is
          const dataMatch = srcBody.match(/url\('data:[^']+'\)/);
          if (dataMatch) return `src: ${dataMatch[0]};`;
          return match; // Keep original if no woff2 found
        }
      );

      combinedCSS += `\n/* ${cssFile} */\n${cssContent}`;
    } catch (err) {
      // Skip CSS files that can't be processed
    }
  }

  return combinedCSS;
}

function rewriteRelativeImports(code, basePath) {
  // Handle static imports: from "./path" or from './path' or from `path`
  const staticImportRegex = /from\s*["'`](\.[^"'`]+)["'`]/g;
  
  // Handle dynamic imports: import("./path") or import('./path') or import(`./path`)
  const dynamicImportRegex = /import\s*\(\s*["'`](\.[^"'`]+)["'`]\s*\)/g;
  
  let rewritten = code;
  
  // Helper to process matches
  const processMatches = (regex, isDynamic) => {
    const matches = [...rewritten.matchAll(regex)];
    matches.forEach(match => {
      const relativePath = match[1];
      // Use posix path joining to ensure forward slashes
      const resolved = join(dirname(basePath), relativePath).replace(/\\/g, '/');
      const normalized = resolved.replace(/^\.\//, '').replace(/\.mjs$/, '.mjs');
      
      if (isDynamic) {
        // Rewrite dynamic import to use normalized path (which import map will resolve)
        rewritten = rewritten.replace(match[0], `import("${normalized}")`);
      } else {
        // Rewrite static import
        rewritten = rewritten.replace(match[0], `from"${normalized}"`);
      }
    });
  };

  processMatches(staticImportRegex, false);
  processMatches(dynamicImportRegex, true);
  
  return rewritten;
}

function stripGlyphReferences(code) {
  // Don't strip glyph references - we want to bundle them!
  return code;
}

async function minifyJS(content, relativePath) {
  // ALWAYS minify - essential for proper bundle size
  try {
    const minified = await minify(content, {
      compress: {
        dead_code: true,
        drop_console: true,
        drop_debugger: true,
        unused: true,
        passes: 3,
        pure_getters: true,
        unsafe: true,
        unsafe_math: true,
        unsafe_proto: true
      },
      mangle: true,
      module: true, // Support top-level await and import/export
      format: { comments: false, ascii_only: false, ecma: 2020 }
    });
    
    if (!minified || !minified.code) {
      console.error(`❌ Minification returned null for ${relativePath}`);
      throw new Error(`Minification failed: no output`);
    }
    
    return minified.code;
  } catch (error) {
    console.error(`❌ CRITICAL: Minification failed for ${relativePath}:`, error.message);
    throw error; // Don't silently return unminified - this causes 3x size bloat!
  }
}

function safeJSONStringify(obj) {
  return JSON.stringify(obj)
    .replace(/<\/script/gi, '\\u003c/script')
    .replace(/\u2028/g, '\\u2028')
    .replace(/\u2029/g, '\\u2029');
}

async function buildMinimalBundle(pieceName, kidlispSource) {
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

  let gitVersion = "unknown";
  try {
    const gitHash = execSync("git rev-parse --short HEAD", { encoding: "utf8" }).trim();
    const gitDirty = execSync("git diff --quiet || echo dirty", { encoding: "utf8" }).trim();
    gitVersion = gitHash + (gitDirty ? " (dirty)" : "");
  } catch (e) {
    // Git might not be available in lambda environment
  }

  const vfs = {};
  
  console.log(`📦 Building bundle for ${pieceName}...`);
  console.log(`📦 Processing ${ESSENTIAL_FILES.length} essential files...`);
  console.log(`📦 Processing ${ESSENTIAL_DEPS.length} dependencies...`);
  
  // Helper to process a list of files
  const processFiles = async (files) => {
    for (const file of files) {
      try {
        let code = await readLocalFile(file);
        if (!code) {
          console.warn(`   ⚠️ Could not load ${file}`);
          continue;
        }
        
        console.log(`   ✓ Loaded ${file} (${code.length} bytes)`);
        
        if (file === 'disks/common/fonts.mjs') {
          code = stripGlyphReferences(code);
        }

        if (file === 'bios.mjs') {
          // Patch bios.mjs to remove font loading since we inline them
          code = code.replace(
            /const fonts = \[\s*"berkeley-mono-variable\.css",[\s\S]*?\];[\s\S]*?fonts\.forEach\(\(font\) => \{[\s\S]*?\}\);/g,
            'console.log("Fonts inlined by export-kidlisp");'
          );
        }
        
        if (file.endsWith('.mjs') || file.endsWith('.js')) {
          code = await minifyJS(code, file);
          code = rewriteRelativeImports(code, file);
        }
        
        vfs[file] = code;
      } catch (error) {
        console.error(`   ❌ Error processing ${file}:`, error.message);
      }
    }
  };

  // Helper to add font glyphs in compact format:
  // {r:[w,h],c:[[x1,y1,x2,y2],...],p:[[x,y],...]} instead of full JSON
  const addFontGlyphs = async (fontName, fontData) => {
    for (const [char, path] of Object.entries(fontData)) {
      if (char.startsWith("glyph") || typeof path !== "string" || path === "false" || path.length === 0) continue;

      let relativePath;
      let vfsPath;

      if (fontName === "font_1") {
        relativePath = `disks/drawings/${fontName}/${path}.json`;
        vfsPath = `aesthetic.computer/${relativePath}`;
      } else if (fontName === "microtype") {
        relativePath = `disks/drawings/${path}.json`;
        vfsPath = `aesthetic.computer/${relativePath}`;
      } else {
        continue;
      }

      try {
        const content = await readLocalFile(relativePath);
        if (content) {
          const data = JSON.parse(content);
          // Compact format: {r:[w,h], c:[[args],...], p:[[args],...]}
          const compact = { r: data.resolution };
          const lines = [], points = [];
          for (const cmd of data.commands || []) {
            if (cmd.name === "point") points.push(cmd.args);
            else lines.push(cmd.args); // "line" is the default
          }
          if (lines.length) compact.c = lines;
          if (points.length) compact.p = points;
          vfs[vfsPath] = JSON.stringify(compact);
        }
      } catch (err) {
        // Skip missing glyph files
      }
    }
  };

  await processFiles(ESSENTIAL_FILES);
  await processFiles(ESSENTIAL_DEPS);
  
  // Add font glyphs (only font_1 has actual glyph files)
  console.log("🔤 Bundling font glyphs...");
  await addFontGlyphs("font_1", font_1);
  // Note: microtype glyphs don't exist as files, font uses fallback rendering

  // Add the piece source code to VFS (single canonical key)
  if (kidlispSource) {
    const cleanName = pieceName.replace(/^\$/, '');
    vfs[`aesthetic.computer/disks/${cleanName}.lisp`] = kidlispSource;
  }

  // Get inlined CSS
  const inlinedCSS = await getInlinedCSS();
  
  // Create import map entries for all modules
  const moduleFiles = Object.keys(vfs).filter(f => 
    f.endsWith('.mjs') || f.endsWith('.js')
  );
  
  const html = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${pieceName} • Aesthetic Computer</title>
  <style>
    body { margin: 0; padding: 0; background: black; overflow: hidden; }
    canvas { display: block; }
    ${inlinedCSS}
  </style>
</head>
<body>
  <script type="module">
    // Pack mode flags
    window.acPACK_MODE = true;
    window.acSTARTING_PIECE = "${pieceName}";
    window.acPACK_PIECE = "${pieceName}";
    window.acPACK_DATE = "${packDate}";
    window.acPACK_GIT_VERSION = "${gitVersion}";
    
    // Pre-cache the piece code for KidLisp (bypasses API fetch in pack mode)
    // Store with the actual piece name used in the system (without $)
    window.objktKidlispCodes = {
      "${pieceName.replace(/^\$/, '')}": ${safeJSONStringify(kidlispSource)},
      "${pieceName}": ${safeJSONStringify(kidlispSource)}
    };
    
    // Virtual File System - all code embedded as strings
    window.VFS = ${safeJSONStringify(vfs)};
    
    // Create import map for module resolution
    const importMap = {
      imports: {}
    };
    
    // Add entries for each module (3 key variations: bare, /, https://)
    const moduleFiles = ${JSON.stringify(moduleFiles).replace(/<\/script/gi, '<\\/script')};
    moduleFiles.forEach(filepath => {
      const blobUrl = URL.createObjectURL(
        new Blob([window.VFS[filepath]], { type: 'application/javascript' })
      );
      importMap.imports[filepath] = blobUrl;
      importMap.imports['/' + filepath] = blobUrl;
      importMap.imports['https://aesthetic.computer/' + filepath] = blobUrl;
    });
    
    // Inject the import map
    const importMapScript = document.createElement('script');
    importMapScript.type = 'importmap';
    importMapScript.textContent = JSON.stringify(importMap);
    document.head.appendChild(importMapScript);
    
    // Intercept fetch for non-module resources
    const originalFetch = window.fetch;
    window.fetch = function(url, options) {
      const urlStr = typeof url === 'string' ? url : url.url;
      
      // Handle direct .lisp file requests (canonical key: clean name without $)
      if (urlStr.includes('.lisp')) {
        const packPieceClean = window.acPACK_PIECE.replace(/^\$/, '');
        if (urlStr.includes(packPieceClean + '.lisp')) {
          const vfsKey = "aesthetic.computer/disks/" + packPieceClean + ".lisp";
          if (window.VFS[vfsKey]) {
            return Promise.resolve(new Response(window.VFS[vfsKey], {
              status: 200,
              headers: { 'Content-Type': 'text/plain' }
            }));
          }
        }
      }

      // Handle font glyph requests — expand compact format back to full JSON
      if (urlStr.includes('/disks/drawings/font_1/') || urlStr.includes('/disks/drawings/microtype/')) {
        const decodedUrl = decodeURIComponent(urlStr);
        const match = decodedUrl.match(/aesthetic\.computer\/disks\/drawings\/.+/);
        if (match) {
          const vfsKey = match[0];
          const raw = window.VFS[vfsKey];
          if (raw) {
            // Expand compact glyph format {r,c,p} → full {resolution,commands}
            let expanded = raw;
            try {
              const d = JSON.parse(raw);
              if (d.r && !d.resolution) {
                const cmds = [];
                if (d.c) for (const a of d.c) cmds.push({name:"line",args:a});
                if (d.p) for (const a of d.p) cmds.push({name:"point",args:a});
                expanded = JSON.stringify({resolution:d.r,commands:cmds});
              }
            } catch(e) {}
            return Promise.resolve(new Response(expanded, {
              status: 200,
              headers: { 'Content-Type': 'application/json' }
            }));
          }
        }
      }

      // Handle store-kidlisp API requests
      if (urlStr.includes('store-kidlisp')) {
        try {
          const urlObj = new URL(urlStr, window.location.origin);
          const requestedCode = urlObj.searchParams.get('code');
          const packPiece = window.acPACK_PIECE.replace(/^\$/, '');
          const cleanRequested = requestedCode ? requestedCode.replace(/^\$/, '') : '';
          if (cleanRequested === packPiece) {
            const vfsKey = "aesthetic.computer/disks/" + packPiece + ".lisp";
            if (window.VFS[vfsKey]) {
              return Promise.resolve(new Response(JSON.stringify({ source: window.VFS[vfsKey], code: requestedCode }), {
                status: 200,
                headers: { 'Content-Type': 'application/json' }
              }));
            }
          }
        } catch (e) {}
      }
      
      // Extract the path from various URL formats
      let path = urlStr;
      
      // Handle full URLs
      try {
        const urlObj = new URL(urlStr, window.location.origin);
        path = urlObj.pathname;
      } catch (e) {
        // Fallback for relative paths
      }

      // Clean up path
      if (path.startsWith('/')) path = path.substring(1);
      
      // Try to match VFS keys
      // 1. Exact match
      if (window.VFS[path]) {
         // Found it
      } else if (!path.startsWith('aesthetic.computer/') && window.VFS['aesthetic.computer/' + path]) {
         // Try adding prefix
         path = 'aesthetic.computer/' + path;
      }

      // Check if we have this resource in VFS
      if (window.VFS[path]) {
        const content = window.VFS[path];
        const blob = new Blob([content], { 
          type: path.endsWith('.mjs') || path.endsWith('.js') 
            ? 'application/javascript' 
            : 'text/plain' 
        });
        return Promise.resolve(new Response(blob, { status: 200 }));
      }
      
      // Fall through to original fetch for external resources
      return originalFetch(url, options);
    };
    
    // Intercept XMLHttpRequest for any code using XHR
    const OriginalXHR = window.XMLHttpRequest;
    window.XMLHttpRequest = function() {
      const xhr = new OriginalXHR();
      const originalOpen = xhr.open;
      
      xhr.open = function(method, url, ...args) {
        let path = url;
        
        // Handle full URLs
        try {
          const urlObj = new URL(url, window.location.origin);
          path = urlObj.pathname;
        } catch (e) {}

        if (path.startsWith('/')) path = path.substring(1);
        
        if (!window.VFS[path] && window.VFS['aesthetic.computer/' + path]) {
           path = 'aesthetic.computer/' + path;
        }
        
        if (window.VFS[path]) {
          // Simulate successful XHR with VFS content
          Object.defineProperty(xhr, 'responseText', {
            get: () => window.VFS[path]
          });
          Object.defineProperty(xhr, 'status', { get: () => 200 });
          Object.defineProperty(xhr, 'readyState', { get: () => 4 });
          
          xhr.send = function() {
            setTimeout(() => {
              if (xhr.onload) xhr.onload();
              if (xhr.onreadystatechange) xhr.onreadystatechange();
            }, 0);
          };
          return;
        }
        
        return originalOpen.call(this, method, url, ...args);
      };
      
      return xhr;
    };
    
    // Boot the system
    import('boot.mjs').catch(error => {
      document.body.style.color='#fff';
      document.body.textContent='Boot failed: '+error.message;
    });
  </script>
</body>
</html>`;

  return html;
}

async function createSelfDecompressingBundle(html, pieceName) {
  // Clean the HTML by removing literal newlines in base64 data URIs
  // This prevents syntax errors when document.write() executes the decompressed content
  const cleanedHtml = html.replace(/url\(['"]data:[^'"]+['"]\)/g, (match) => {
    return match.replace(/\r?\n/g, '');
  });
  
  const compressed = await gzip(cleanedHtml, { level: 9 });
  const base64 = compressed.toString('base64');
  
  // Fetch pako library to inline it (for offline support)
  let pakoCode;
  try {
    const httpClient = await getHttpClient();
    pakoCode = await httpClient.get('https://cdn.jsdelivr.net/npm/pako@2.1.0/dist/pako.min.js').text();
    console.log(`📦 Inlined pako library (${pakoCode.length} bytes)`);
  } catch (e) {
    console.error('⚠️ Failed to fetch pako library:', e.message);
    throw new Error('Cannot create self-decompressing bundle without pako library');
  }
  
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${pieceName} • Aesthetic Computer</title>
  <style>body{margin:0;background:#000;overflow:hidden}</style>
</head>
<body>
  <script>${pakoCode}</script>
  <script>
    // Use blob: URL instead of data: URL for CSP compatibility (objkt sandboxing)
    const b64='${base64}';
    const bin=atob(b64);
    const bytes=new Uint8Array(bin.length);
    for(let i=0;i<bin.length;i++)bytes[i]=bin.charCodeAt(i);
    const h=pako.inflate(bytes,{to:'string'});
    document.open();document.write(h);document.close();
  </script>
</body>
</html>`;
}

async function createGzipNativeSelfDecompressingBundle(html, pieceName) {
  const gzipCompress = promisify(zlib.gzip);
  const compressed = await gzipCompress(Buffer.from(html, 'utf-8'), { level: 9 });
  const base64 = compressed.toString('base64');

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>${pieceName} • Aesthetic Computer</title>
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
      .then(h=>{URL.revokeObjectURL(url);document.open();document.write(h);document.close();});
  </script>
</body>
</html>`;
}

export async function handler(event, context) {
  try {
    const { piece, format = 'gzip-native' } = event.queryStringParameters || {};
    
    if (!piece) {
      return respond(400, { error: 'Missing piece parameter. Usage: ?piece=$pie (KidLisp) or ?piece=line (.mjs)' });
    }

    const isKidLisp = piece.startsWith('$');
    const cleanPiece = piece.replace(/^\$/, '');
    console.log(`📦 Exporting ${piece} (type: ${isKidLisp ? 'KidLisp' : '.mjs'}, name: ${cleanPiece})...`);

    let source;
    if (isKidLisp) {
      // Fetch KidLisp source from store-kidlisp API
      source = await fetchKidLispSource(cleanPiece);
      if (!source) {
        console.error(`📦 No KidLisp source found for ${piece}`);
        return respond(404, { error: `KidLisp source not found for ${piece}. Try saving it with $code first.` });
      }
    } else {
      // Fetch .mjs piece source from production
      source = await fetchMjsPieceSource(cleanPiece);
      if (!source) {
        console.error(`📦 No .mjs piece found for ${piece}`);
        return respond(404, { error: `.mjs piece not found: ${piece}. Check that disks/${cleanPiece}.mjs exists.` });
      }
    }

    console.log(`📦 Building bundle for ${piece}...`);
    // Build the bundle (buildMinimalBundle will handle both types)
    const html = await buildMinimalBundle(piece, source);
    
    let finalHtml, filename, size;
    
    if (format === 'gzip-native') {
      finalHtml = await createGzipNativeSelfDecompressingBundle(html, piece);
      filename = `${cleanPiece}.html`;
      size = Math.round(finalHtml.length / 1024);
      console.log(`✅ Gzip native self-decompressing bundle: ${size} KB`);
    } else if (format === 'gzip' || format === 'selfdecompressing') {
      finalHtml = await createSelfDecompressingBundle(html, piece);
      filename = `${cleanPiece}-gzip.html`;
      size = Math.round(finalHtml.length / 1024);
      console.log(`✅ Gzip self-decompressing bundle: ${size} KB`);
    } else {
      finalHtml = html;
      filename = `${cleanPiece}.html`;
      size = Math.round(html.length / 1024);
      console.log(`✅ Standard bundle: ${size} KB`);
    }

    return {
      statusCode: 200,
      headers: {
        'Content-Type': 'text/html; charset=utf-8',
        'Content-Disposition': `attachment; filename="${filename}"`,
        'X-Bundle-Size-KB': size.toString(),
      },
      body: finalHtml
    };

  } catch (error) {
    console.error('📦 Export error:', error);
    return respond(500, { error: error.message, stack: error.stack });
  }
}
