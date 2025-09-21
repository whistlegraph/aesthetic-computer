#!/usr/bin/env node

// ac-pack: Package aesthetic.computer pieces for Teia Interactive OBJKTs
// Usage: node ac-pack.mjs <piece-name> [options]

import { promises as fs } from "fs";
import fsSync from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { spawn } from "child_process";
import { extractCodes, fetchAllCodes, generateCacheCode } from "./kidlisp-extractor.mjs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Configuration
const SYSTEM_DIR = path.join(__dirname, "..", "system");
const PUBLIC_DIR = path.join(SYSTEM_DIR, "public");
const AC_DIR = path.join(PUBLIC_DIR, "aesthetic.computer");
const DISKS_DIR = path.join(AC_DIR, "disks");
const TOKENS_DIR = path.join(__dirname, "output");

class AcPacker {
  constructor(pieceName, options = {}) {
    this.pieceName = pieceName;
    this.options = {
      outputDir: path.join(TOKENS_DIR, pieceName),
      coverImage: options.coverImage || "cover.svg",
      title: options.title || pieceName,
      description: options.description || `Interactive ${pieceName} piece from aesthetic.computer`,
      author: options.author || "aesthetic.computer",
      ...options,
    };
    this.bundledFiles = new Set();
  }

  async pack() {
    console.log(`📦 Packing ${this.pieceName} for Teia...`);
    
    try {
      await this.createOutputDir();
      const pieceData = await this.loadPiece();
      
      // Handle KidLisp dependencies if this is a KidLisp piece
      if (pieceData.isKidLispCode) {
        await this.bundleKidLispDependencies(pieceData);
      }
      
      await this.generateIndexHtml(pieceData);
      await this.bundleSystemFiles();
      await this.bundleLibFiles();
      await this.bundleSystemsFiles();
      await this.bundleDepFiles();
      await this.bundleCommonDiskFiles();
      await this.bundleCurrentPiece();
      await this.createDiskStubs();
      await this.createAssets();
      await this.bundleWebfonts();
      await this.bundleFontAssets();
      await this.generateCover();
      await this.copyAssets();
      
      console.log("✅ Successfully generated assets for", this.pieceName);
      console.log("📁 Files bundled:", this.bundledFiles.size);
      console.log("📍 Location:", this.options.outputDir);
      
      return { success: true, outputDir: this.options.outputDir };
    } catch (error) {
      console.error("❌ Packing failed:", error);
      return { success: false, error };
    }
  }

  async createOutputDir() {
    await fs.mkdir(this.options.outputDir, { recursive: true });
    console.log(`📁 Created output directory: ${this.options.outputDir}`);
  }

  async loadPiece() {
    // First check if this is a KidLisp $code
    if (this.pieceName.startsWith('$')) {
      const codeId = this.pieceName.slice(1); // Remove $ prefix
      console.log(`🔍 Detected KidLisp code: ${this.pieceName}`);
      
      // Fetch the code and all its dependencies
      const { fetchCode } = await import("./kidlisp-extractor.mjs");
      const result = await fetchCode(codeId);
      
      if (result) {
        console.log(`📜 Loaded KidLisp code: ${this.pieceName}`);
        return { 
          sourceCode: result.source, 
          language: "kidlisp", 
          metadata: {},
          isKidLispCode: true,
          codeId: codeId
        };
      } else {
        throw new Error(`KidLisp code not found: ${this.pieceName}`);
      }
    }
    
    // Try to load JavaScript piece first
    const jsPath = path.join(DISKS_DIR, `${this.pieceName}.mjs`);
    try {
      const sourceCode = await fs.readFile(jsPath, "utf8");
      console.log(`📜 Loaded javascript piece: ${this.pieceName}`);
      return { sourceCode, language: "javascript", metadata: {} };
    } catch (err) {
      // Try Lisp piece if JavaScript fails
      const lispPath = path.join(DISKS_DIR, `${this.pieceName}.lisp`);
      try {
        const sourceCode = await fs.readFile(lispPath, "utf8");
        console.log(`📜 Loaded lisp piece: ${this.pieceName}`);
        return { sourceCode, language: "lisp", metadata: {} };
      } catch (lispErr) {
        throw new Error(`Piece not found: ${this.pieceName} (tried .mjs, .lisp, and $code API)`);
      }
    }
  }

  async bundleKidLispDependencies(pieceData) {
    console.log(`🔗 Fetching KidLisp dependencies for ${this.pieceName}...`);
    
    // Fetch all nested dependencies
    const codesMap = await fetchAllCodes(pieceData.sourceCode);
    
    if (codesMap.size > 0) {
      console.log(`📚 Found ${codesMap.size} KidLisp dependencies`);
      
      // Generate cache preload code
      const cacheCode = generateCacheCode(codesMap);
      
      // Save cache file
      const cacheFilePath = path.join(this.options.outputDir, "kidlisp-cache.js");
      await fs.writeFile(cacheFilePath, cacheCode);
      this.bundledFiles.add("kidlisp-cache.js");
      
      console.log(`💾 Saved KidLisp cache to kidlisp-cache.js`);
    } else {
      console.log(`ℹ️ No KidLisp dependencies found`);
    }
  }

  async generateIndexHtml(pieceData) {
    const { metadata } = pieceData;
    
    const indexHtml = `<!doctype html>
<html>
  <head>
    <meta charset="utf-8" />
    <base href="./" />
    <title>${metadata.title || this.options.title}</title>
    <meta property="og:image" content="${this.options.coverImage}" />
    <link rel="icon" href="./aesthetic.computer/favicon.png" type="image/png" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />
    <meta name="description" content="${this.options.description}" />
    <meta name="og:title" content="${metadata.title || this.options.title}" />
    <meta name="og:description" content="${this.options.description}" />
    <meta name="twitter:card" content="summary_large_image" />
    <meta name="twitter:title" content="${metadata.title || this.options.title}" />
    <meta name="twitter:site" content="${this.options.author}" />
    <meta name="twitter:image" content="${this.options.coverImage}" />
    
    <script type="text/javascript">
// Teia mode configuration - simple starting piece override
window.acTEIA_MODE = true;
window.acSTARTING_PIECE = "${this.pieceName}"; // Override default "prompt" piece

// Extract Teia URL parameters
const urlParams = new URLSearchParams(window.location.search);
window.acTEIA_VIEWER = urlParams.get('viewer') || null;
window.acTEIA_CREATOR = urlParams.get('creator') || null;

// Force sandbox mode for Teia
window.acSANDBOX_MODE = true;

// Disable session for teia mode (no need for session in standalone packages)
window.acDISABLE_SESSION = true;

// Enable nogap mode by default for teia
window.acNOGAP_MODE = true;

console.log('🎭 Teia mode activated:', {
  startingPiece: window.acSTARTING_PIECE,
  viewer: window.acTEIA_VIEWER,
  creator: window.acTEIA_CREATOR,
  disableSession: window.acDISABLE_SESSION,
  nogapMode: window.acNOGAP_MODE
});
    </script>
    ${pieceData.isKidLispCode ? '<script src="./kidlisp-cache.js"></script>' : ''}
    <script src="./aesthetic.computer/boot.mjs" type="module" defer></script>
    <link rel="stylesheet" href="./aesthetic.computer/style.css" />
  </head>
  <body class="native-cursor">
    <div id="console" class="hidden">booting...</div>
    <script>
      if (window.self !== window.top) document.body.classList.add("embed");
      
      // Auto-enable nogap for teia mode or URL parameter
      const params = new URLSearchParams(location.search);
      if (window.acNOGAP_MODE || params.has("nogap") || location.search.includes("nogap")) {
        document.body.classList.add("nogap");
      }
    </script>
  </body>
</html>`;

    await fs.writeFile(path.join(this.options.outputDir, "index.html"), indexHtml);
    console.log("📄 Generated index.html");
  }

  async bundleSystemFiles() {
    const acOutputDir = path.join(this.options.outputDir, "aesthetic.computer");
    await fs.mkdir(acOutputDir, { recursive: true });

    const coreFiles = ["boot.mjs", "style.css", "bios.mjs"];

    for (const file of coreFiles) {
      const srcPath = path.join(AC_DIR, file);
      const destPath = path.join(acOutputDir, file);
      
      try {
        let content = await fs.readFile(srcPath, "utf8");
        
        // Patch style.css for better nogap support in teia mode
        if (file === 'style.css') {
          content = this.patchStyleCssForTeia(content);
          console.log(`🔧 Patched style.css for enhanced nogap support`);
        }
        
        // Patch bios.mjs for teia mode - fix webfont URLs
        if (file === 'bios.mjs') {
          content = await this.patchBiosJsForTeia(content);
          console.log(`🎨 Patched bios.mjs for teia mode`);
        }
        
        // Patch boot.mjs for teia mode - fix dependency URLs
        if (file === 'boot.mjs') {
          content = await this.patchBootJsForTeia(content);
          console.log(`🎨 Patched boot.mjs for teia mode`);
        }
        
        await fs.writeFile(destPath, content);
        this.bundledFiles.add(file);
        console.log(`📎 Bundled: ${file}`);
      } catch (error) {
        console.warn(`⚠️ Warning: Could not bundle ${file}:`, error.message);
      }
    }
  }

  async bundleLibFiles() {
    const libDir = path.join(AC_DIR, "lib");
    const libOutputDir = path.join(this.options.outputDir, "aesthetic.computer", "lib");
    await fs.mkdir(libOutputDir, { recursive: true });

    const libFiles = await fs.readdir(libDir);
    const jsFiles = libFiles.filter(file => file.endsWith(".mjs"));

    console.log(`📚 Found ${jsFiles.length} library files to bundle`);

    for (const libFile of jsFiles) {
      try {
        const srcPath = path.join(libDir, libFile);
        const destPath = path.join(libOutputDir, libFile);
        let content = await fs.readFile(srcPath, "utf8");
        
        // Patch type.mjs for teia mode - prevent API fallback calls
        if (libFile === 'type.mjs') {
          content = this.patchTypeJsForTeia(content);
          console.log(`🔧 Patched type.mjs for teia mode`);
        }
        
        // Patch kidlisp.mjs for teia mode - reduce verbose logging
        if (libFile === 'kidlisp.mjs') {
          content = this.patchKidLispJsForTeia(content);
          console.log(`🔧 Patched kidlisp.mjs for teia mode`);
        }
        
        // Patch disk.mjs for teia mode - prevent session connections
        if (libFile === 'disk.mjs') {
          content = await this.patchDiskJsForTeia(content);
          console.log(`🔧 Patched disk.mjs for teia mode`);
        }
        
        await fs.writeFile(destPath, content);
        this.bundledFiles.add(`lib/${libFile}`);
        console.log(`📚 Bundled lib: ${libFile}`);
      } catch (error) {
        console.warn(`⚠️ Warning: Could not bundle lib/${libFile}:`, error.message);
      }
    }

    // Handle subdirectories (like sound/, glazes/)
    const allEntries = await fs.readdir(libDir, { withFileTypes: true });
    const subdirs = allEntries.filter(entry => entry.isDirectory()).map(entry => entry.name);

    for (const subdir of subdirs) {
      const subdirPath = path.join(libDir, subdir);
      const subdirOutputPath = path.join(libOutputDir, subdir);
      await fs.mkdir(subdirOutputPath, { recursive: true });

      const subdirFiles = await fs.readdir(subdirPath);
      for (const file of subdirFiles) {
        if (file.endsWith(".mjs") || file.endsWith(".js")) {
          const srcPath = path.join(subdirPath, file);
          const destPath = path.join(subdirOutputPath, file);
          const content = await fs.readFile(srcPath, "utf8");
          await fs.writeFile(destPath, content);
          this.bundledFiles.add(`lib/${subdir}/${file}`);
          console.log(`📚 Bundled lib/${subdir}: ${file}`);
        }
      }
    }

    // Create required stubs
    const stubs = [
      { name: "uniforms.js", content: "// Uniform stub for Teia mode\nexport default {};" },
      { name: "vec4.mjs", content: "// Vec4 stub for Teia mode\nexport default {};" },
      { name: "idb.js", content: "// IndexedDB stub for Teia mode" },
      { name: "geckos.io-client.2.3.2.min.js", content: "// Geckos stub for Teia mode" }
    ];

    for (const stub of stubs) {
      const stubPath = path.join(libOutputDir, stub.name);
      await fs.writeFile(stubPath, stub.content);
      console.log(`📝 Created additional stub: ${stub.name}`);
    }
  }

  async bundleSystemsFiles() {
    const systemsDir = path.join(AC_DIR, "systems");
    const systemsOutputDir = path.join(this.options.outputDir, "aesthetic.computer", "systems");
    await fs.mkdir(systemsOutputDir, { recursive: true });

    try {
      const systemFiles = await fs.readdir(systemsDir);
      const jsFiles = systemFiles.filter(file => file.endsWith(".mjs"));

      console.log(`⚙️ Found ${jsFiles.length} system files to bundle`);

      for (const systemFile of jsFiles) {
        const srcPath = path.join(systemsDir, systemFile);
        const destPath = path.join(systemsOutputDir, systemFile);
        const content = await fs.readFile(srcPath, "utf8");
        await fs.writeFile(destPath, content);
        this.bundledFiles.add(`system: ${systemFile}`);
        console.log(`⚙️ Bundled system: ${systemFile}`);
      }
    } catch (error) {
      console.warn("⚠️ Warning: Could not bundle system files:", error.message);
    }
  }

  async bundleDepFiles() {
    const depDir = path.join(AC_DIR, "dep");
    const depOutputDir = path.join(this.options.outputDir, "aesthetic.computer", "dep");
    
    try {
      await fs.mkdir(depOutputDir, { recursive: true });
      const depFiles = await this.getAllFilesRecursively(depDir);
      
      console.log(`📦 Bundling ${depFiles.length} dependency files...`);

      for (const file of depFiles) {
        const relativePath = path.relative(depDir, file);
        const destPath = path.join(depOutputDir, relativePath);
        const destDir = path.dirname(destPath);
        
        await fs.mkdir(destDir, { recursive: true });
        const content = await fs.readFile(file, "utf8");
        await fs.writeFile(destPath, content);
        this.bundledFiles.add(`dep: ${relativePath}`);
        console.log(`📦 Bundled dep: ${relativePath}`);
      }
    } catch (error) {
      console.warn("⚠️ Warning: Could not bundle dependency files:", error.message);
    }
  }

  async bundleCommonDiskFiles() {
    const commonDiskDir = path.join(DISKS_DIR, "common");
    const commonOutputDir = path.join(this.options.outputDir, "aesthetic.computer", "disks", "common");

    try {
      await fs.mkdir(commonOutputDir, { recursive: true });
      const commonFiles = await fs.readdir(commonDiskDir);
      const jsFiles = commonFiles.filter(file => file.endsWith(".mjs"));

      console.log(`📚 Found ${jsFiles.length} common disk files to bundle`);

      for (const commonFile of jsFiles) {
        const srcPath = path.join(commonDiskDir, commonFile);
        const destPath = path.join(commonOutputDir, commonFile);
        const content = await fs.readFile(srcPath, "utf8");
        await fs.writeFile(destPath, content);
        this.bundledFiles.add(`common disk: ${commonFile}`);
        console.log(`📚 Bundled common disk: ${commonFile}`);
      }
    } catch (error) {
      console.warn("⚠️ Warning: Could not bundle common disk files:", error.message);
    }
  }

  async bundleCurrentPiece() {
    const disksOutputDir = path.join(this.options.outputDir, "aesthetic.computer", "disks");
    await fs.mkdir(disksOutputDir, { recursive: true });

    // Handle KidLisp $codes - create a stub piece that jumps to the cached code
    if (this.pieceName.startsWith('$')) {
      const stubContent = `// KidLisp $code stub for Teia mode
export function boot({ wipe, ink, help }) {
  // Load the cached KidLisp code
  wipe("black");
  ink("white");
  help.choose("Load ${this.pieceName} from cache...").then(() => {
    help.system.nopaint.load("${this.pieceName}");
  });
}`;
      
      const destPath = path.join(disksOutputDir, `${this.pieceName}.mjs`);
      await fs.writeFile(destPath, stubContent);
      this.bundledFiles.add(`KidLisp stub: ${this.pieceName}.mjs`);
      console.log(`🔗 Created KidLisp stub for: ${this.pieceName}`);
      return;
    }

    // Copy the current piece to the disks directory (regular pieces)
    const extensions = [".mjs", ".lisp"];
    for (const ext of extensions) {
      const srcPath = path.join(DISKS_DIR, `${this.pieceName}${ext}`);
      const destPath = path.join(disksOutputDir, `${this.pieceName}${ext}`);
      
      try {
        const content = await fs.readFile(srcPath, "utf8");
        await fs.writeFile(destPath, content);
        this.bundledFiles.add(`current piece: ${this.pieceName}${ext}`);
        console.log(`🎯 Bundled current piece: ${this.pieceName}${ext}`);
        break; // Stop after first successful copy
      } catch (error) {
        // Try next extension
      }
    }
  }

  async createDiskStubs() {
    const disksOutputDir = path.join(this.options.outputDir, "aesthetic.computer", "disks");
    
    // Create essential disk stubs that might be required
    const stubs = [
      { name: "chat.mjs", content: "// Chat stub for Teia mode\nexport function boot() {}\nexport function paint() {}\nexport function act() {}" }
    ];

    for (const stub of stubs) {
      const stubPath = path.join(disksOutputDir, stub.name);
      await fs.writeFile(stubPath, stub.content);
      console.log(`📝 Created disk stub: ${stub.name}`);
    }
  }

  async createAssets() {
    // Create favicon
    const faviconPath = path.join(this.options.outputDir, "aesthetic.computer", "favicon.png");
    // Create a minimal 1x1 PNG as placeholder
    const minimalPng = Buffer.from([
      0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00, 0x00, 0x00, 0x0D,
      0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
      0x08, 0x06, 0x00, 0x00, 0x00, 0x1F, 0x15, 0xC4, 0x89, 0x00, 0x00, 0x00,
      0x0B, 0x49, 0x44, 0x41, 0x54, 0x78, 0x9C, 0x63, 0x00, 0x01, 0x00, 0x00,
      0x05, 0x00, 0x01, 0x0D, 0x0A, 0x2D, 0xB4, 0x00, 0x00, 0x00, 0x00, 0x49,
      0x45, 0x4E, 0x44, 0xAE, 0x42, 0x60, 0x82
    ]);
    await fs.writeFile(faviconPath, minimalPng);
    console.log("🖼️ Created favicon.png");
  }

  async bundleWebfonts() {
    const webfontsDir = path.join(__dirname, "..", "system", "public", "assets", "type", "webfonts");
    const webfontsOutputDir = path.join(this.options.outputDir, "type", "webfonts");

    try {
      await fs.mkdir(webfontsOutputDir, { recursive: true });
      const webfontFiles = await fs.readdir(webfontsDir);

      console.log("🔤 Bundling webfonts...");

      for (const fontFile of webfontFiles) {
        const srcPath = path.join(webfontsDir, fontFile);
        const destPath = path.join(webfontsOutputDir, fontFile);
        
        if (fontFile.endsWith(".css") || fontFile.endsWith(".woff2") || fontFile.endsWith(".woff") || fontFile.endsWith(".ttf")) {
          const content = await fs.readFile(srcPath);
          await fs.writeFile(destPath, content);
          console.log(`🔤 Bundled webfont: ${fontFile}`);
        }
      }
    } catch (error) {
      console.warn("⚠️ Warning: Could not bundle webfonts:", error.message);
    }
  }

  async bundleFontAssets() {
    const fontsDir = path.join(__dirname, "..", "system", "public", "assets", "type");
    const assetsOutputDir = path.join(this.options.outputDir, "assets", "type");

    try {
      await fs.mkdir(assetsOutputDir, { recursive: true });
      
      // Bundle MatrixChunky8 precomputed glyphs
      const matrixFontDir = path.join(fontsDir, "MatrixChunky8");
      const matrixOutputDir = path.join(assetsOutputDir, "MatrixChunky8");
      
      await fs.mkdir(matrixOutputDir, { recursive: true });
      
      const glyphFiles = await fs.readdir(matrixFontDir);
      const jsonFiles = glyphFiles.filter(file => file.endsWith(".json"));
      
      console.log(`🔤 Found ${jsonFiles.length} MatrixChunky8 glyph files to bundle`);
      
      let bundledCount = 0;
      for (const glyphFile of jsonFiles) {
        const srcPath = path.join(matrixFontDir, glyphFile);
        
        // Convert 2-digit hex filename to 4-digit hex for consistency with code expectations
        const hexCode = glyphFile.replace('.json', '');
        const paddedHexCode = hexCode.padStart(4, '0').toUpperCase();
        const destFileName = `${paddedHexCode}.json`;
        const destPath = path.join(matrixOutputDir, destFileName);
        
        const content = await fs.readFile(srcPath, "utf8");
        await fs.writeFile(destPath, content);
        bundledCount++;
      }
      
      console.log(`✅ Successfully bundled ${bundledCount} MatrixChunky8 glyph files`);
      
      // Also create a font manifest for easier debugging
      const fontManifest = {
        font: "MatrixChunky8",
        bundledGlyphs: jsonFiles.map(file => {
          const hexCode = file.replace('.json', '');
          const charCode = parseInt(hexCode, 16);
          return {
            hex: hexCode,
            decimal: charCode,
            char: String.fromCharCode(charCode),
            file: file
          };
        }),
        totalGlyphs: bundledCount,
        bundledAt: new Date().toISOString()
      };
      
      const manifestPath = path.join(matrixOutputDir, "_manifest.json");
      await fs.writeFile(manifestPath, JSON.stringify(fontManifest, null, 2));
      console.log(`📋 Created font manifest: _manifest.json`);
      
    } catch (error) {
      console.error("❌ Error bundling font assets:", error.message);
      console.error("   Make sure the font directory exists at:", fontsDir);
    }
  }

  async generateCover() {
    const coverPath = path.join(this.options.outputDir, "cover.svg");
    const coverSvg = `<svg xmlns="http://www.w3.org/2000/svg" width="400" height="400" viewBox="0 0 400 400">
  <rect width="400" height="400" fill="#000"/>
  <text x="200" y="200" text-anchor="middle" dominant-baseline="middle" fill="#fff" font-family="monospace" font-size="20">${this.pieceName}</text>
</svg>`;
    await fs.writeFile(coverPath, coverSvg);
    console.log("🖼️ Generated placeholder cover: cover.svg");
  }

  async copyAssets() {
    console.log("📎 Asset copying complete (minimal set)");
  }

  patchStyleCssForTeia(content) {
    // Enhance nogap mode for full viewport coverage
    console.log("🔧 Patching style.css for enhanced nogap support...");
    
    // Find the existing nogap rules and enhance them
    const nogapBodyPattern = /body\.nogap \{[^}]*\}/;
    const nogapComputerPattern = /body\.nogap #aesthetic-computer \{[^}]*\}/;
    
    // Enhanced nogap body rules
    const enhancedNogapBody = `body.nogap {
  background-image: none;
  background-color: transparent !important;
  margin: 0 !important;
  padding: 0 !important;
  width: 100vw !important;
  height: 100vh !important;
  overflow: hidden !important;
}`;

    // Enhanced nogap computer element rules  
    const enhancedNogapComputer = `body.nogap #aesthetic-computer {
  border-radius: 0px;
  position: fixed !important;
  top: 0 !important;
  left: 0 !important;
  width: 100vw !important;
  height: 100vh !important;
  margin: 0 !important;
  padding: 0 !important;
}`;

    let patched = content;
    
    // Replace existing nogap rules or add them if they don't exist
    if (nogapBodyPattern.test(content)) {
      patched = patched.replace(nogapBodyPattern, enhancedNogapBody);
    } else {
      // Add the rule after the body rule
      patched = patched.replace(/body \{[^}]*\}/, match => match + '\n\n' + enhancedNogapBody);
    }
    
    if (nogapComputerPattern.test(content)) {
      patched = patched.replace(nogapComputerPattern, enhancedNogapComputer);
    } else {
      // Add after the #aesthetic-computer rule
      patched = patched.replace(/#aesthetic-computer \{[^}]*\}/, match => match + '\n\n' + enhancedNogapComputer);
    }
    
    return patched;
  }

  patchTypeJsForTeia(content) {
    console.log("🔧 Patching type.mjs for teia mode...");
    
    // The original type.mjs already has proper TEIA mode handling for MatrixChunky8 fonts
    // We just need to fix the filename format to not strip leading zeros since our bundled files keep them
    let patched = content;
    
    // Update the filename generation to keep leading zeros for bundled files
    const localFileNamePattern = /const localFileName = codePointStr\.split\('_'\)\.map\(code => \{\s*\/\/ Remove leading zeros but keep at least one digit\s*return code\.replace\(\/\^0\+\/, ''\) \|\| '0';\s*\}\)\.join\('_'\);/;
    patched = patched.replace(localFileNamePattern, 
      `// For bundled files, keep the original hex format with leading zeros
      const localFileName = codePointStr;`
    );
    
    return patched;
  }

  async patchBootJsForTeia(content) {
    console.log('🎨 Patching boot.mjs for teia dependency URLs and TV mode...');
    
    let patched = content;
    
    // Force TV mode (non-interactive) when in TEIA mode
    const tvParamPattern = /const tv = tvParam === true \|\| tvParam === "true";/;
    patched = patched.replace(tvParamPattern, 
      `const tv = tvParam === true || tvParam === "true" || window.acTEIA_MODE;`
    );
    
    // Fix auth0 script URL to use relative path in teia mode
    const auth0Pattern = /script\.src = "\/aesthetic\.computer\/dep\/auth0-spa-js\.production\.js";/;
    patched = patched.replace(auth0Pattern, 
      `// Check if we're in teia mode for relative path
      const isTeiaMode = (typeof window !== 'undefined' && window.acTEIA_MODE) ||
                       (typeof globalThis !== 'undefined' && globalThis.acTEIA_MODE);
      
      if (isTeiaMode) {
        script.src = "./aesthetic.computer/dep/auth0-spa-js.production.js";
      } else {
        script.src = "/aesthetic.computer/dep/auth0-spa-js.production.js";
      }`
    );
    
    return patched;
  }

  patchKidLispJsForTeia(content) {
    console.log("🔧 Patching kidlisp.mjs for teia mode...");
    
    // Since we've already cleaned up the original source files, 
    // this function can now be simplified or removed entirely
    return content;
  }

  async patchDiskJsForTeia(content) {
    console.log('🔧 Patching disk.mjs for Teia mode...');
    
    let patched = content;
    
    // Add teia mode check to prevent session connections
    const socketPattern = /if \(\s*\/\/parsed\.search\?\.startsWith\("preview"\) \|\|\s*\/\/parsed\.search\?\.startsWith\("icon"\)\s*previewOrIconMode\s*\) \{/;
    patched = patched.replace(socketPattern, 
      `if (
      //parsed.search?.startsWith("preview") ||
      //parsed.search?.startsWith("icon")
      previewOrIconMode ||
      (typeof window !== 'undefined' && window.acTEIA_MODE) ||
      (typeof globalThis !== 'undefined' && globalThis.acTEIA_MODE)
    ) {`
    );
    
    return patched;
  }

  async patchHeadersJsForTeia(content) {
    console.log('🎨 Adding TEIA mode console styling...');
    
    let patched = content;
    
    // Add TEIA console log after the entire console.log statement for Aesthetic Computer
    const headerPattern = /(console\.log\(\s*"%cAesthetic Computer",\s*`[^`]*`\s*\);\s*\/\/ Print a pretty title in the console\.)/;
    patched = patched.replace(headerPattern, 
      '$1\n\n  // Add TEIA mode indicator if enabled\n' +
      '  if ((typeof window !== "undefined" && window.acTEIA_MODE) ||\n' +
      '      (typeof globalThis !== "undefined" && globalThis.acTEIA_MODE)) {\n' +
      '    console.log(\n' +
      '      "%cTEIA",\n' +
      '      "background: rgba(40, 40, 40);" +\n' +
      '      "color: rgb(200, 220, 200);" +\n' +
      '      "font-size: 14px;" +\n' +
      '      "padding: 0 0.5em;" +\n' +
      '      "border-radius: 0.15em;" +\n' +
      '      "border: 1px solid rgb(120, 150, 120);" +\n' +
      '      "margin-left: 0.25em;"\n' +
      '    );\n' +
      '  }'
    );
    
    return patched;
  }

  async patchBiosJsForTeia(content) {
    console.log('🎨 Patching bios.mjs for teia webfont URLs...');
    
    let patched = content;
    
    // Replace the font URL logic to use relative paths in teia mode
    const fontUrlPattern = /\/\/ Use origin-aware font loading\s*let fontUrl;\s*try \{[\s\S]*?link\.href = fontUrl;/;
    patched = patched.replace(fontUrlPattern, 
      `// Use origin-aware font loading with teia mode support
        let fontUrl;
        try {
          // Check if we're in teia mode
          const isTeiaMode = (typeof window !== 'undefined' && window.acTEIA_MODE) ||
                           (typeof globalThis !== 'undefined' && globalThis.acTEIA_MODE);
          
          if (isTeiaMode) {
            // In teia mode, use relative path to bundled webfonts
            fontUrl = "./type/webfonts/" + font;
          } else {
            // Check if we're in development environment
            const isDevelopment = location.hostname === 'localhost' && location.port;
            if (isDevelopment) {
              // In development, fonts are served from the root /type/webfonts/ path
              fontUrl = "/type/webfonts/" + font;
            } else {
              // In production or sandboxed iframe, use the standard path
              fontUrl = "/type/webfonts/" + font;
            }
          }
        } catch (err) {
          // Fallback to standard path if there's any error
          fontUrl = "/type/webfonts/" + font;
        }
        
        link.href = fontUrl;`
    );
    
    // Also patch the worker path to use relative paths in teia mode
    const workerPathPattern = /const fullPath =\s*"\/aesthetic\.computer\/lib\/disk\.mjs"\s*\+\s*window\.location\.search\s*\+\s*"#"\s*\+\s*Date\.now\(\);/;
    patched = patched.replace(workerPathPattern, 
      `const fullPath = (typeof window !== 'undefined' && window.acTEIA_MODE) ? 
        "./aesthetic.computer/lib/disk.mjs" + "#" + Date.now() : 
        "/aesthetic.computer/lib/disk.mjs" + window.location.search + "#" + Date.now();`
    );
    
    // Also patch the initial piece parsing to prioritize acSTARTING_PIECE in teia mode
    const pieceParsingPattern = /const parsed = parse\(sluggy \|\| window\.acSTARTING_PIECE\);/;
    patched = patched.replace(pieceParsingPattern, 
      `const parsed = parse((typeof window !== 'undefined' && window.acTEIA_MODE && window.acSTARTING_PIECE) ? 
        window.acSTARTING_PIECE : 
        (sluggy || window.acSTARTING_PIECE));`
    );
    
    return patched;
  }

  async patchParseJsForTeia(content) {
    console.log('🎨 Patching parse.mjs for teia mode piece override...');
    
    let patched = content;
    
    // Override slug function to return acSTARTING_PIECE in teia mode
    const slugFunctionPattern = /function slug\(url\) \{[\s\S]*?return cleanedUrl;\s*\}/;
    patched = patched.replace(slugFunctionPattern, 
      `function slug(url) {
  // In teia mode, always prioritize acSTARTING_PIECE over URL parsing
  if ((typeof window !== 'undefined' && window.acTEIA_MODE && window.acSTARTING_PIECE)) {
    return window.acSTARTING_PIECE;
  }
  
  //console.log("🐛 slug() input:", url);
  
  // Remove http protocol and host from current url before feeding it to parser.
  let cleanedUrl = url
    .replace(/^http(s?):\/\//i, "")
    .replace(window.location.hostname + ":" + window.location.port + "/", "")
    .replace(window.location.hostname + "/", "")
    .split("#")[0]; // Remove any hash.

  //console.log("🐛 slug() after host removal:", cleanedUrl);

  // Use safe parameter removal instead of .split("?")[0] 
  cleanedUrl = getCleanPath(cleanedUrl);
  
  //console.log("🐛 slug() after getCleanPath:", cleanedUrl);

  // Decode URL-encoded characters first
  cleanedUrl = decodeURIComponent(cleanedUrl);
  
  //console.log("🐛 slug() after decodeURIComponent:", cleanedUrl);

  // Only apply kidlisp URL decoding if this actually looks like kidlisp code
  if (isKidlispSource(cleanedUrl)) {
    //console.log("🐛 slug() detected as KidLisp, decoding...");
    return decodeKidlispFromUrl(cleanedUrl);
  }
  
  //console.log("🐛 slug() final result:", cleanedUrl);
  return cleanedUrl;
}`
    );
    
    return patched;
  }

  async getAllFilesRecursively(dir) {
    const files = [];
    const entries = await fs.readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        files.push(...await this.getAllFilesRecursively(fullPath));
      } else {
        files.push(fullPath);
      }
    }

    return files;
  }
}

// Main execution
async function main() {
  const pieceName = process.argv[2];
  
  if (!pieceName) {
    console.error("Usage: node ac-pack-simple.mjs <piece-name>");
    process.exit(1);
  }

  const packer = new AcPacker(pieceName);
  const result = await packer.pack();
  
  if (!result.success) {
    console.error("Packing failed:", result.error);
    process.exit(1);
  }

  // Interactive testing and zip creation
  console.log("");
  console.log("🎉 Package assets generated successfully!");
  console.log(`📁 Directory: ${packer.options.outputDir}`);
  console.log("");
  
  // Auto-create zip with timestamp
  console.log("� Package ready for Teia deployment!");
  console.log("   • All assets bundled locally");
  console.log("   • Font loading fixed for offline use");
  console.log("   • Session connections disabled");
  console.log("   • TEIA mode styling enabled");
  console.log("");
  
  // Automatically create zip with timestamp
  console.log("📦 Creating zip file...");
  try {
    const zipPath = await createZipWithTimestamp(packer.options.outputDir, packer.pieceName);
    console.log("");
    console.log("🎉 Success! Your package is ready for Teia:");
    console.log(`📁 Directory: ${packer.options.outputDir}`);
    console.log(`📦 Zip file: ${zipPath}`);
    console.log("");
    console.log("🚀 Next steps:");
    console.log("1. Go to https://teia.art/mint");
    console.log(`2. Upload ${path.basename(zipPath)}`);
    console.log("3. Preview and test your interactive OBJKT");
    console.log("4. Mint when ready!");
    console.log("");
    
    // Clean up build artifacts (keep only the zip)
    console.log("🧹 Cleaning up build artifacts...");
    await fs.rm(packer.options.outputDir, { recursive: true, force: true });
    console.log("✅ Build directory cleaned up");
  } catch (error) {
    console.error("❌ Failed to create zip:", error.message);
    process.exit(1);
  }
}

async function createZipWithTimestamp(outputDir, pieceName) {
  const archiver = (await import('archiver')).default;
  
  // Create timestamp for the zip filename
  const now = new Date();
  const timestamp = now.toISOString().replace(/[:.]/g, '-').split('.')[0]; // Format: YYYY-MM-DDTHH-MM-SS
  const zipPath = `${outputDir}-${timestamp}.zip`;
  
  const output = fsSync.createWriteStream(zipPath);
  const archive = archiver('zip', { zlib: { level: 9 } });

  return new Promise((resolve, reject) => {
    output.on('close', () => {
      console.log(`📦 Created zip: ${zipPath} (${archive.pointer()} bytes)`);
      resolve(zipPath);
    });

    archive.on('error', reject);
    archive.pipe(output);
    archive.directory(outputDir, false);
    archive.finalize();
  });
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
