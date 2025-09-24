#!/usr/bin/env node

// ac-pack: Package aesthetic.computer pieces for Teia Interactive OBJKTs
// Usage: node ac-pack.mjs <piece-name> [options]

import { promises as fs } from "fs";
import fsSync from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { spawn } from "child_process";
import { extractCodes, fetchAllCodes, generateCacheCode } from "./kidlisp-extractor.mjs";
import { timestamp } from "../system/public/aesthetic.computer/lib/num.mjs";
import { execSync } from "child_process";
import os from "os";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Get git information for colophonic data
function getGitInfo() {
  try {
    const gitDir = path.join(__dirname, "..");
    const commit = execSync("git rev-parse HEAD", { cwd: gitDir, encoding: "utf8" }).trim();
    const shortCommit = commit.substring(0, 7);
    const branch = execSync("git rev-parse --abbrev-ref HEAD", { cwd: gitDir, encoding: "utf8" }).trim();
    const commitDate = execSync("git show -s --format=%ci HEAD", { cwd: gitDir, encoding: "utf8" }).trim();
    const isDirty = execSync("git status --porcelain", { cwd: gitDir, encoding: "utf8" }).trim().length > 0;
    
    return {
      commit,
      shortCommit,
      branch,
      commitDate,
      isDirty,
      repoUrl: "https://github.com/digitpain/aesthetic.computer"
    };
  } catch (error) {
    console.log("ℹ️ Could not retrieve git information:", error.message);
    return {
      commit: "unknown",
      shortCommit: "unknown",
      branch: "unknown", 
      commitDate: "unknown",
      isDirty: false,
      repoUrl: "https://github.com/digitpain/aesthetic.computer"
    };
  }
}

// Configuration
const SYSTEM_DIR = path.join(__dirname, "..", "system");
const PUBLIC_DIR = path.join(SYSTEM_DIR, "public");
const AC_DIR = path.join(PUBLIC_DIR, "aesthetic.computer");
const DISKS_DIR = path.join(AC_DIR, "disks");
const TOKENS_DIR = path.join(__dirname, "output");

class AcPacker {
  constructor(pieceName, options = {}) {
    this.pieceName = pieceName;
    this.zipTimestamp = timestamp(); // Generate timestamp once for consistent naming
    // Sanitize piece name for directory creation (remove $ and other shell-problematic characters)
    const sanitizedPieceName = pieceName.replace(/[$]/g, '');
    this.options = {
      outputDir: path.join(TOKENS_DIR, sanitizedPieceName),
      coverImage: options.coverImage || "cover.gif", // Default to GIF, fallback to SVG handled in generateCover
      title: options.title || pieceName,
      description: options.description || `Interactive ${pieceName} piece from aesthetic.computer`,
      author: options.author || "@jeffrey",
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
      let hasDependencies = false;
      if (pieceData.isKidLispCode) {
        hasDependencies = await this.bundleKidLispDependencies(pieceData);
      }
      
      await this.bundleSystemFiles();
      await this.bundleLibFiles();
      await this.bundleSystemsFiles();
      await this.bundleDepFiles();
      await this.bundleCommonDiskFiles();
      await this.bundleFontDrawings(); // Add font drawings bundling
      await this.bundleCurrentPiece();
      await this.createDiskStubs();
      await this.createAssets();
      await this.bundleWebfonts();
      await this.bundleFontAssets();
      await this.generateCover();
      await this.copyAssets();
      
      // Generate index.html after all bundling is complete so we have accurate file count
      await this.generateIndexHtml(pieceData, hasDependencies);
      
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
    
    // Create a codes map that includes the main code itself
    const codesMap = await fetchAllCodes(pieceData.sourceCode);
    
    // Always include the main piece code in the cache
    const mainCodeId = pieceData.codeId; // "roz" from "$roz"
    codesMap.set(mainCodeId, {
      source: pieceData.sourceCode,
      when: new Date().toISOString(),
      hits: 1,
      user: "teia-package"
    });
    
    console.log(`📚 Found ${codesMap.size} KidLisp codes (including main: ${mainCodeId})`);
    
    // Always generate cache code if we have any codes (including main)
    if (codesMap.size > 0) {
      // Store the cache data for inline injection into HTML
      this.kidlispCacheData = { codesMap, count: codesMap.size };
      console.log(`💾 Prepared KidLisp cache for inline injection with ${codesMap.size} codes`);
      return true; // Dependencies + main code bundled
    } else {
      console.log(`ℹ️ No KidLisp codes to bundle`);
      return false; // No codes at all
    }
  }

  async generateIndexHtml(pieceData, hasDependencies = false) {
    // Set up TEIA mode environment before generating metadata
    global.window = global.window || {};
    global.window.acTEIA_MODE = true;
    global.globalThis = global.globalThis || {};
    global.globalThis.acTEIA_MODE = true;
    
    // Import and call metadata with TEIA context
    const { metadata } = await import("../system/public/aesthetic.computer/lib/parse.mjs");
    const teiaContext = { author: this.options.author };
    const generatedMetadata = metadata("localhost", this.pieceName, {}, "https:", teiaContext);
    
    // Override metadata URLs to use relative paths for static packaging
    if (generatedMetadata) {
      generatedMetadata.icon = `./icon/128x128/${this.pieceName}.png`;
      // Also override any preview/cover images to use our static cover
      generatedMetadata.ogImage = this.options.coverImage;
      generatedMetadata.twitterImage = this.options.coverImage;
      // Use relative manifest path for standalone packages
      generatedMetadata.manifest = "./manifest.json";
    }
    
    const gitInfo = getGitInfo();
    console.log("🔧 Git info retrieved:", gitInfo);
    const packTime = new Date().toISOString();
    
    // Get system information
    const systemInfo = {
      platform: process.platform,
      arch: process.arch,
      nodeVersion: process.version,
      hostname: os.hostname(),
      userInfo: os.userInfo().username
    };
    
    // Prepare colophonic information
    const zipFilename = `${this.options.author}-${this.pieceName}-${this.zipTimestamp}.zip`;
    
    const colophonData = {
      piece: {
        name: this.pieceName,
        isKidLisp: pieceData.isKidLispCode,
        sourceCode: pieceData.sourceCode || null,
        hasDependencies,
        codeLength: pieceData.sourceCode ? pieceData.sourceCode.length : 0
      },
      build: {
        packTime,
        author: this.options.author,
        gitCommit: gitInfo.shortCommit,
        gitCommitFull: gitInfo.commit,
        gitBranch: gitInfo.branch,
        gitCommitDate: gitInfo.commitDate,
        gitIsDirty: gitInfo.isDirty,
        repoUrl: gitInfo.repoUrl,
        systemInfo,
        fileCount: this.bundledFiles.size,
        zipFilename: zipFilename
      },
      metadata: generatedMetadata || {}
    };
    
    console.log("📋 Colophon data prepared:", JSON.stringify(colophonData, null, 2));
    
    const indexHtml = `<!doctype html>
<html>
  <head>
    <meta charset="utf-8" />
    <base href="./" />
    <title>${generatedMetadata.title || this.options.title}</title>
    <meta property="og:image" content="${this.options.coverImage}" />
    <link rel="icon" href="./aesthetic.computer/favicon.png" type="image/png" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />
    <meta name="description" content="${this.options.description}" />
    <meta name="og:title" content="${generatedMetadata.title || this.options.title}" />
    <meta name="og:description" content="${this.options.description}" />
    <meta name="twitter:card" content="summary_large_image" />
    <meta name="twitter:title" content="${generatedMetadata.title || this.options.title}" />
    <meta name="twitter:site" content="${this.options.author}" />
    <meta name="twitter:image" content="${this.options.coverImage}" />
    
    <script type="text/javascript">
// Teia mode configuration - simple starting piece override
window.acTEIA_MODE = true;
window.acSTARTING_PIECE = "${this.pieceName}"; // Override default "prompt" piece

// Colophonic information for provenance and debugging
window.acTEIA_COLOPHON = ${JSON.stringify(colophonData, null, 2)};

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

// Set TEIA mode on both window and globalThis for maximum compatibility
window.acTEIA_MODE = true;
globalThis.acTEIA_MODE = true;

console.log('🎭 Teia mode activated:', {
  startingPiece: window.acSTARTING_PIECE,
  viewer: window.acTEIA_VIEWER,
  creator: window.acTEIA_CREATOR,
  disableSession: window.acDISABLE_SESSION,
  nogapMode: window.acNOGAP_MODE,
  teiaMode: window.acTEIA_MODE
});

// Periodically ensure TEIA mode stays enabled
setInterval(() => {
  if (!window.acTEIA_MODE || !globalThis.acTEIA_MODE) {
    console.log('� Restoring TEIA mode flags');
    window.acTEIA_MODE = true;
    globalThis.acTEIA_MODE = true;
  }
}, 100);
    </script>
    ${this.generateKidLispCacheScript()}
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
    
    // Generate a basic manifest.json for the packaged piece
    const manifest = {
      name: generatedMetadata.title || this.options.title,
      short_name: this.pieceName,
      start_url: "./",
      display: "standalone",
      background_color: "#000000",
      theme_color: "#0084FF",
      icons: [
        {
          src: `./icon/128x128/${this.pieceName}.png`,
          sizes: "128x128",
          type: "image/png"
        }
      ]
    };
    
    await fs.writeFile(path.join(this.options.outputDir, "manifest.json"), JSON.stringify(manifest, null, 2));
    console.log("📄 Generated manifest.json");
  }

  generateKidLispCacheScript() {
    // Return empty string if no cache data
    if (!this.kidlispCacheData || !this.kidlispCacheData.codesMap) {
      return '';
    }

    // Generate the inline cache script
    const cacheCode = generateCacheCode(this.kidlispCacheData.codesMap);
    return `<script type="text/javascript">
${cacheCode}
    </script>`;
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
        
        // Patch headers.mjs for teia mode - fix import statements
        if (libFile === 'headers.mjs') {
          content = this.patchHeadersJsForTeia(content);
          console.log(`🔧 Patched headers.mjs for teia mode`);
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

  async bundleFontDrawings() {
    const drawingsDir = path.join(DISKS_DIR, "drawings");
    const drawingsOutputDir = path.join(this.options.outputDir, "aesthetic.computer", "disks", "drawings");

    try {
      await fs.mkdir(drawingsOutputDir, { recursive: true });
      
      // Bundle font_1 directory
      const font1Dir = path.join(drawingsDir, "font_1");
      const font1OutputDir = path.join(drawingsOutputDir, "font_1");
      
      if (await this.directoryExists(font1Dir)) {
        await fs.mkdir(font1OutputDir, { recursive: true });
        
        // Copy all subdirectories and files
        const font1Subdirs = await fs.readdir(font1Dir);
        let totalGlyphs = 0;
        
        for (const subdir of font1Subdirs) {
          const srcSubdirPath = path.join(font1Dir, subdir);
          const destSubdirPath = path.join(font1OutputDir, subdir);
          
          const stat = await fs.stat(srcSubdirPath);
          if (stat.isDirectory()) {
            await fs.mkdir(destSubdirPath, { recursive: true });
            
            // Copy all .json files in this subdirectory
            const files = await fs.readdir(srcSubdirPath);
            const jsonFiles = files.filter(file => file.endsWith(".json"));
            
            for (const jsonFile of jsonFiles) {
              const srcFilePath = path.join(srcSubdirPath, jsonFile);
              const destFilePath = path.join(destSubdirPath, jsonFile);
              const content = await fs.readFile(srcFilePath, "utf8");
              await fs.writeFile(destFilePath, content);
              totalGlyphs++;
            }
          }
        }
        
        this.bundledFiles.add(`font_1 glyphs: ${totalGlyphs} files`);
        console.log(`🔤 Bundled font_1: ${totalGlyphs} glyph files`);
      } else {
        console.log("ℹ️ font_1 directory not found, skipping");
      }
      
    } catch (error) {
      console.warn("⚠️ Warning: Could not bundle font drawings:", error.message);
    }
  }

  async directoryExists(dir) {
    try {
      const stat = await fs.stat(dir);
      return stat.isDirectory();
    } catch {
      return false;
    }
  }

  async bundleCurrentPiece() {
    const disksOutputDir = path.join(this.options.outputDir, "aesthetic.computer", "disks");
    await fs.mkdir(disksOutputDir, { recursive: true });

    // Handle KidLisp $codes - create a stub piece that jumps to the cached code
    if (this.pieceName.startsWith('$')) {
      const stubContent = `// KidLisp $code stub for Teia mode
export function boot({ wipe, ink, help, backgroundFill }) {
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
    const webfontsDir = path.join(__dirname, "..", "system", "public", "type", "webfonts");
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
      
      // Check if MatrixChunky8 directory exists before trying to read it
      try {
        await fs.access(matrixFontDir);
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
        
      } catch (matrixError) {
        console.log("ℹ️ MatrixChunky8 font directory not found, skipping font bundling");
      }
      
    } catch (error) {
      console.error("❌ Error bundling font assets:", error.message);
      console.error("   Make sure the font directory exists at:", fontsDir);
    }
  }

  async generateCover() {
    // Try to generate an animated GIF cover using the piece
    try {
      const { RenderOrchestrator } = await import("../reference/tools/recording/orchestrator.mjs");
      const coverPath = path.join(this.options.outputDir, "cover.gif");
      
      // Create a temporary output directory for GIF generation
      const tempOutputDir = path.join(this.options.outputDir, ".temp-gif");
      await fs.mkdir(tempOutputDir, { recursive: true });
      
      console.log("🎞️ Generating animated GIF cover...");
      console.log(`📁 Using piece: ${this.pieceName}`);
      console.log(`📁 Base outputDir: ${this.options.outputDir}`);
      console.log(`📁 Temp outputDir: ${tempOutputDir}`);
      
      // Use the RenderOrchestrator to generate GIF
      const orchestrator = new RenderOrchestrator(
        this.pieceName,     // piece (supports KidLisp $code or .mjs files)
        360,                // 6 seconds at 60fps (360 frames) for cover
        tempOutputDir,      // temporary output directory
        400,                // width
        400,                // height
        { gifMode: true }   // enable GIF mode
      );
      
      // Run the rendering
      await orchestrator.renderAll();
      
      // Find the generated GIF file and move it to cover.gif  
      // The GIF will be created in the same directory as tempOutputDir
      const searchDir = this.options.outputDir;
      const tempFiles = await fs.readdir(searchDir);
      const gifFile = tempFiles.find(file => file.endsWith('.gif') && file.includes(this.pieceName.replace('$', '')));
      
      if (gifFile) {
        const sourcePath = path.join(searchDir, gifFile);
        await fs.copyFile(sourcePath, coverPath);
        
        // Create external copy with zip naming pattern
        const externalCoverFilename = `${this.options.author}-${this.pieceName}-${this.zipTimestamp}-cover.gif`;
        const externalCoverPath = path.join(path.dirname(this.options.outputDir), externalCoverFilename);
        await fs.copyFile(sourcePath, externalCoverPath);
        
        // Clean up temporary files
        await fs.rm(tempOutputDir, { recursive: true, force: true });
        await fs.unlink(sourcePath).catch(() => {}); // Ignore errors
        
        console.log("🎞️ Generated animated cover: cover.gif");
        console.log(`🖼️ External cover created: ${externalCoverFilename}`);
        this.options.coverImage = "cover.gif";
        return;
      }
    } catch (error) {
      console.warn("⚠️ GIF generation failed, falling back to SVG:", error.message);
      console.warn("   Error details:", error.stack);
    }
    
    // Fallback to SVG cover if GIF generation fails
    const coverPath = path.join(this.options.outputDir, "cover.svg");
    const coverSvg = `<svg xmlns="http://www.w3.org/2000/svg" width="400" height="400" viewBox="0 0 400 400">
  <rect width="400" height="400" fill="#000"/>
  <text x="200" y="200" text-anchor="middle" dominant-baseline="middle" fill="#fff" font-family="monospace" font-size="20">${this.pieceName}</text>
</svg>`;
    await fs.writeFile(coverPath, coverSvg);
    console.log("🖼️ Generated fallback cover: cover.svg");
    this.options.coverImage = "cover.svg";
  }

  async copyAssets() {
    // Copy cursor files
    try {
      const cursorsSourceDir = path.join(AC_DIR, "cursors");
      const cursorsOutputDir = path.join(this.options.outputDir, "aesthetic.computer", "cursors");
      
      await fs.mkdir(cursorsOutputDir, { recursive: true });
      
      // Copy all cursor files
      const cursorFiles = await fs.readdir(cursorsSourceDir);
      
      for (const file of cursorFiles) {
        if (file.endsWith('.svg')) {
          const sourcePath = path.join(cursorsSourceDir, file);
          const outputPath = path.join(cursorsOutputDir, file);
          await fs.copyFile(sourcePath, outputPath);
          console.log(`📎 Bundled cursor: ${file}`);
        }
      }
    } catch (error) {
      console.log("ℹ️ Cursor directory error:", error.message);
    }
    
    // Generate stub icon (128x128 PNG)
    try {
      const iconDir = path.join(this.options.outputDir, "icon", "128x128");
      await fs.mkdir(iconDir, { recursive: true });
      
      const iconPath = path.join(iconDir, `${this.pieceName}.png`);
      
      // Create a proper 128x128 PNG with transparent background
      console.log(`🖼️ Generating stub icon for ${this.pieceName}...`);
      
      // Create a simple 128x128 PNG with aesthetic.computer-style colors
      const width = 128;
      const height = 128;
      
      // PNG file signature + IHDR chunk + simple transparent image data + IEND
      const pngHeader = Buffer.from([
        0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, // PNG signature
        0x00, 0x00, 0x00, 0x0D, // IHDR chunk length
        0x49, 0x48, 0x44, 0x52, // IHDR chunk type
        0x00, 0x00, 0x00, 0x80, // Width: 128
        0x00, 0x00, 0x00, 0x80, // Height: 128
        0x08, 0x06, 0x00, 0x00, 0x00, // 8-bit RGBA, no compression/filter/interlace
        0xC3, 0x3E, 0x61, 0xCB // IHDR CRC
      ]);
      
      // Create minimal IDAT chunk with solid color (aesthetic blue: #0084FF)
      const pixelData = Buffer.alloc(width * height * 4); // RGBA
      for (let i = 0; i < pixelData.length; i += 4) {
        pixelData[i] = 0x00;     // R
        pixelData[i + 1] = 0x84; // G  
        pixelData[i + 2] = 0xFF; // B
        pixelData[i + 3] = 0xFF; // A (fully opaque)
      }
      
      // For simplicity, create a minimal transparent PNG
      const simpleTransparentPng = Buffer.from([
        0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, // PNG signature
        0x00, 0x00, 0x00, 0x0D, // IHDR length
        0x49, 0x48, 0x44, 0x52, // IHDR
        0x00, 0x00, 0x00, 0x80, // width: 128
        0x00, 0x00, 0x00, 0x80, // height: 128
        0x08, 0x06, 0x00, 0x00, 0x00, // 8-bit RGBA
        0xC3, 0x3E, 0x61, 0xCB, // CRC
        0x00, 0x00, 0x00, 0x17, // IDAT length
        0x49, 0x44, 0x41, 0x54, // IDAT
        0x78, 0x9C, 0xED, 0xC1, 0x01, 0x01, 0x00, 0x00, 0x00, 0x80, 0x90, 0xFE,
        0xAF, 0x6E, 0x48, 0x40, 0x00, 0x00, 0x00, 0x02, 0x10, 0x00, 0x01,
        0x8E, 0x0D, 0x71, 0xDA, // IDAT data + CRC
        0x00, 0x00, 0x00, 0x00, // IEND length
        0x49, 0x45, 0x4E, 0x44, // IEND
        0xAE, 0x42, 0x60, 0x82  // IEND CRC
      ]);
      
      await fs.writeFile(iconPath, simpleTransparentPng);
      console.log(`🖼️ Created stub icon: icon/128x128/${this.pieceName}.png (128x128px transparent PNG)`);
      
    } catch (error) {
      console.log("ℹ️ Error creating icon stub:", error.message);
    }
    
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
    
    // The original type.mjs already has proper TEIA mode handling for fonts
    // No additional patching needed as of the latest source code
    return content;
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
    // kidlisp.mjs now has built-in TEIA support via getCachedCodeMultiLevel
    // No patching needed anymore
    return content;
  }  async patchDiskJsForTeia(content) {
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

  patchHeadersJsForTeia(content) {
    console.log('🎨 Patching headers.mjs for teia import statements...');
    
    let patched = content;
    
    // Replace the import statement with stub functions that preserve functionality
    // but don't rely on external module loading
    const stubFunctions = `
// Inlined color-highlighting functions for TEIA mode (to avoid import 404s)
function colorizeColorName(colorName) {
  // Simple colorization - just return the color name for now in TEIA mode
  return colorName;
}

function getColorTokenHighlight(token) {
  // Basic color highlighting for common KidLisp tokens
  const colorMap = {
    'purple': '#9575cd',
    'blue': '#42a5f5',
    'red': '#ef5350',
    'green': '#66bb6a',
    'yellow': '#ffee58',
    'orange': '#ff7043',
    'pink': '#ec407a',
    'cyan': '#26c6da',
    'ink': '#90a4ae',
    'line': '#78909c',
    'blur': '#607d8b'
  };
  
  return colorMap[token] || null;
}`;
    
    // Replace the import statement with the stub functions
    patched = patched.replace(
      /^import\s+\{[^}]+\}\s+from\s+"\.\/color-highlighting\.mjs";?\s*$/m,
      '// Import replaced with inline functions for TEIA mode' + stubFunctions
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
    const zipResult = await createZipWithTimestamp(packer.options.outputDir, packer.pieceName, packer.zipTimestamp, packer.options.author);
    console.log("");
    console.log("🎉 Success! Your package is ready for Teia:");
    console.log(`📁 Directory: ${packer.options.outputDir}`);
    console.log(`📦 Zip file: ${zipResult.zipPath}`);
    console.log("");
    console.log("🚀 Next steps:");
    console.log("1. Go to https://teia.art/mint");
    console.log(`2. Upload ${path.basename(zipResult.zipPath)}`);
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

async function createZipWithTimestamp(outputDir, pieceName, timeStr, author = "@jeffrey") {
  const archiver = (await import('archiver')).default;
  
  // Use the provided timestamp for consistency and construct path properly
  const zipPath = path.join(path.dirname(outputDir), `${author}-${pieceName}-${timeStr}.zip`);
  
  const output = fsSync.createWriteStream(zipPath);
  const archive = archiver('zip', { zlib: { level: 9 } });

  return new Promise((resolve, reject) => {
    output.on('close', () => {
      const sizeBytes = archive.pointer();
      const sizeMB = (sizeBytes / (1024 * 1024)).toFixed(2);
      console.log(`📦 Created zip: ${zipPath} (${sizeBytes} bytes / ${sizeMB} MB)`);
      resolve({ zipPath, sizeBytes });
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
