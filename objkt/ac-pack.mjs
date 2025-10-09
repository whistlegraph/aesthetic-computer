#!/usr/bin/env node

// ac-pack: Package aesthetic.computer pieces for Teia Interactive OBJKTs
// Usage: node ac-pack.mjs <piece-name> [options]

import { promises as fs } from "fs";
import fsSync from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { spawn } from "child_process";
import { extractCodes, fetchAllCodes, generateCacheCode } from "./kidlisp-extractor.mjs";
import { execSync } from "child_process";
import os from "os";
import { once } from "events";
import readline from "readline";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const DEFAULT_TIME_ZONE =
  process.env.AC_PACK_TZ || process.env.TZ || "America/Los_Angeles";

// Helper function for interactive prompts
function askQuestion(query) {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  return new Promise(resolve => rl.question(query, answer => {
    rl.close();
    resolve(answer);
  }));
}

function getTimestampParts(date = new Date(), timeZone = DEFAULT_TIME_ZONE) {
  const formatter = new Intl.DateTimeFormat("en-CA", {
    timeZone,
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    hour12: false,
    fractionalSecondDigits: 3,
  });

  const parts = formatter.formatToParts(date).reduce((acc, part) => {
    if (part.type !== "literal") {
      acc[part.type] = part.value;
    }
    return acc;
  }, {});

  if (!parts.fractionalSecond) {
    parts.fractionalSecond = String(date.getMilliseconds()).padStart(3, "0");
  }

  return parts;
}

function formatTimestampForFile(date = new Date(), timeZone = DEFAULT_TIME_ZONE) {
  const parts = getTimestampParts(date, timeZone);
  return `${parts.year}.${parts.month}.${parts.day}.${parts.hour}.${parts.minute}.${parts.second}.${parts.fractionalSecond}`;
}

function formatDateTimeForDisplay(date = new Date(), timeZone = DEFAULT_TIME_ZONE) {
  const formatter = new Intl.DateTimeFormat("en-US", {
    timeZone,
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    hour12: true,
    timeZoneName: "short",
  });

  return formatter.format(date);
}

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
const PACKAGED_SYSTEM_DIR_NAME = "ac";

class AcPacker {
  constructor(pieceName, options = {}) {
    this.pieceName = pieceName;
    this.timeZone = options.timeZone || DEFAULT_TIME_ZONE;
    this.buildDate = new Date();
    this.zipTimestamp = formatTimestampForFile(this.buildDate, this.timeZone); // Generate timestamp once for consistent naming
    this.packagedSystemDirName = options.packagedSystemDirName || PACKAGED_SYSTEM_DIR_NAME;
    this.packagedSystemBaseHref = `./${this.packagedSystemDirName}`;
    // Sanitize piece name for directory creation (remove $ and other shell-problematic characters)
    const sanitizedPieceName = pieceName.replace(/[$]/g, '');
    const rawCoverDuration = options.coverDurationSeconds ?? options.gifLengthSeconds;
    let coverDurationSeconds = Number(rawCoverDuration);
    if (!Number.isFinite(coverDurationSeconds) || coverDurationSeconds <= 0) {
      coverDurationSeconds = 3;
    }
    const coverFrameCount = Math.max(1, Math.round(coverDurationSeconds * 60));
    this.options = {
      ...options,
      outputDir: path.join(options.targetDir || TOKENS_DIR, sanitizedPieceName),
      targetDir: options.targetDir || TOKENS_DIR, // Directory where final artifacts should be placed
      coverImage: options.coverImage || "cover.gif", // Default to GIF, fallback to SVG handled in generateCover
      faviconImage: options.faviconImage || `${this.packagedSystemBaseHref}/favicon.png`, // Default favicon, updated to GIF if available
      title: options.title || pieceName,
      description: options.description || `Interactive ${pieceName} piece from aesthetic.computer`,
      author: options.author || "@jeffrey",
      verbose: options.verbose === true,
      logInkColors: options.logInkColors === true,
      coverDurationSeconds,
      coverFrameCount,
      timeZone: this.timeZone,
      packagedSystemDirName: this.packagedSystemDirName,
      tapePath: options.tapePath ? path.resolve(options.tapePath) : undefined,
    };
    this.bundledFiles = new Set();
    this.tempDirs = [];
  }

  getPackagedDirPath(...segments) {
    return path.join(this.options.outputDir, this.packagedSystemDirName, ...segments);
  }

  getPackagedRelativePath(...segments) {
    return `./${path.posix.join(this.packagedSystemDirName, ...segments)}`;
  }

  logVerbose(...args) {
    if (this.options.verbose) {
      console.log(...args);
    }
  }

  async getGifEncoderClass() {
    if (!this.GIFEncoderClass) {
      const gifModule = await import("gif-encoder-2");
      const GIFEncoder = gifModule.default || gifModule.GIFEncoder || gifModule;
      if (!GIFEncoder) {
        throw new Error("Failed to load gif-encoder-2 module");
      }
      this.GIFEncoderClass = GIFEncoder;
    }
    return this.GIFEncoderClass;
  }

  async getPNGClass() {
    if (!this.PNGClass) {
      const pngModule = await import("pngjs");
      const PNG = pngModule.PNG || (pngModule.default && pngModule.default.PNG) || pngModule.default;
      if (!PNG || !PNG.sync || !PNG.sync.read || !PNG.sync.write) {
        throw new Error("Failed to load pngjs PNG reader");
      }
      this.PNGClass = PNG;
    }
    return this.PNGClass;
  }

  scaleImageNearest(srcData, srcWidth, srcHeight, targetWidth, targetHeight) {
    const output = Buffer.alloc(targetWidth * targetHeight * 4);

    for (let y = 0; y < targetHeight; y++) {
      const srcY = Math.min(srcHeight - 1, Math.floor(((y + 0.5) * srcHeight) / targetHeight));
      for (let x = 0; x < targetWidth; x++) {
        const srcX = Math.min(srcWidth - 1, Math.floor(((x + 0.5) * srcWidth) / targetWidth));
        const srcIndex = (srcY * srcWidth + srcX) * 4;
        const destIndex = (y * targetWidth + x) * 4;

        output[destIndex] = srcData[srcIndex];
        output[destIndex + 1] = srcData[srcIndex + 1];
        output[destIndex + 2] = srcData[srcIndex + 2];
        output[destIndex + 3] = srcData[srcIndex + 3];
      }
    }

    return output;
  }

  async writePngFromRaw(rawData, width, height, outputPath) {
    const PNG = await this.getPNGClass();
    const png = new PNG({ width, height });
    rawData.copy(png.data);
    const buffer = PNG.sync.write(png);
    await fs.writeFile(outputPath, buffer);
  }

  async pack() {
    console.log(`📦 Packing ...`);
    
    try {
      await this.createOutputDir();
      await this.prepareTapeSource();
      const pieceData = await this.loadPiece();
      
      // Store piece data for dependency analysis
      this.pieceData = pieceData;
      
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
      await this.bundleWebfonts();
      await this.bundleFontAssets();
      await this.generateCover();
      await this.createAssets(); // Moved after generateCover to check for GIF favicon first
      await this.copyAssets();
      
      // Generate index.html after all bundling is complete so we have accurate file count
  await this.generateIndexHtml(pieceData, hasDependencies);
  await this.convertMjsModulesToJs();
      
      console.log("✅ Successfully generated assets for", this.pieceName);
      console.log("📁 Files bundled:", this.bundledFiles.size);
      console.log("📍 Location:", this.options.outputDir);
      
      return { success: true, outputDir: this.options.outputDir };
    } catch (error) {
      console.error("❌ Packing failed:", error);
      return { success: false, error };
    } finally {
      await this.cleanupTempDirs();
    }
  }

  async cleanup() {
    // Only clean up if we're using a different target directory than teia/output
    if (this.options.targetDir !== TOKENS_DIR) {
      try {
        await fs.rm(this.options.outputDir, { recursive: true, force: true });
        console.log(`🧹 Cleaned up temporary directory: ${this.options.outputDir}`);
      } catch (error) {
        console.warn(`⚠️ Failed to clean up temporary directory: ${error.message}`);
      }
    }
  }

  async cleanupTempDirs() {
    while (this.tempDirs.length > 0) {
      const dir = this.tempDirs.pop();
      if (!dir) {
        continue;
      }
      try {
        await fs.rm(dir, { recursive: true, force: true });
        this.logVerbose(`🧹 Removed temporary directory: ${dir}`);
      } catch (error) {
        console.warn(`⚠️ Failed to remove temporary directory ${dir}: ${error.message}`);
      }
    }
  }

  async createOutputDir() {
    try {
      await fs.rm(this.options.outputDir, { recursive: true, force: true });
      console.log(`🧹 Cleared existing output directory: ${this.options.outputDir}`);
    } catch (error) {
      console.warn(`⚠️ Failed to clear output directory before pack: ${error.message}`);
    }
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
    // Set up PACK mode environment before generating metadata
    global.window = global.window || {};
    global.window.acPACK_MODE = true;
    global.globalThis = global.globalThis || {};
    global.globalThis.acPACK_MODE = true;
    
    // Import and call metadata with OBJKT context
    const { metadata } = await import("../system/public/aesthetic.computer/lib/parse.mjs");
    const objktContext = { author: this.options.author };
    const generatedMetadata = metadata("localhost", this.pieceName, {}, "https:", objktContext);
    
    // Override metadata URLs to use relative paths for static packaging
    if (generatedMetadata) {
      generatedMetadata.icon = `./icon/256x256/${this.pieceName}.png`;
      // Also override any preview/cover images to use our static cover
      generatedMetadata.ogImage = this.options.coverImage;
      generatedMetadata.twitterImage = this.options.coverImage;
      // Use relative manifest path for standalone packages
      generatedMetadata.manifest = "./manifest.json";
    }
    
    const gitInfo = getGitInfo();
    console.log("🔧 Git info retrieved:", gitInfo);
    const buildDate = this.buildDate ? new Date(this.buildDate) : new Date();
    const packTimeUTC = buildDate.toISOString();
    const packTimeLocal = formatDateTimeForDisplay(buildDate, this.timeZone);
    
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
        packTime: packTimeLocal,
        packTimeLocal,
        packTimeUTC,
        timeZone: this.timeZone,
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
      metadata: {
        ...generatedMetadata,
        favicon: this.options.faviconImage || this.getPackagedRelativePath('favicon.png')
      }
    };
    
    // Set colophon in global context for metadata generation
    globalThis.acPACK_COLOPHON = colophonData;
    
    // Regenerate metadata with complete colophon data (including zipFilename) for proper title
    const finalMetadata = metadata("localhost", this.pieceName, colophonData, "https:", objktContext);
    
    // Override metadata URLs to use relative paths for static packaging
    if (finalMetadata) {
      finalMetadata.icon = `./icon/256x256/${this.pieceName}.png`;
      finalMetadata.ogImage = this.options.coverImage;
      finalMetadata.twitterImage = this.options.coverImage;
      finalMetadata.manifest = "./manifest.json";
    }
    
    // Update colophon with final metadata
    colophonData.metadata = {
      ...finalMetadata,
      favicon: this.options.faviconImage || this.getPackagedRelativePath('favicon.png')
    };
    
    console.log("📋 Colophon data prepared:", JSON.stringify(colophonData, null, 2));
    
    const indexHtml = `<!doctype html>
<html>
  <head>
    <meta charset="utf-8" />
    <base href="./" />
    <title>${finalMetadata.title || this.options.title}</title>
    <meta property="og:image" content="${this.options.coverImage}" />
  <link rel="icon" href="${this.options.faviconImage || this.getPackagedRelativePath('favicon.png')}" type="${this.options.faviconImage && this.options.faviconImage.endsWith('.gif') ? 'image/gif' : 'image/png'}" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />
    <meta name="description" content="${this.options.description}" />
    <meta name="og:title" content="${finalMetadata.title || this.options.title}" />
    <meta name="og:description" content="${this.options.description}" />
    <meta name="twitter:card" content="summary_large_image" />
    <meta name="twitter:title" content="${finalMetadata.title || this.options.title}" />
    <meta name="twitter:site" content="${this.options.author}" />
    <meta name="twitter:image" content="${this.options.coverImage}" />
    
    <script type="text/javascript">
// PACK mode configuration - simple starting piece override
window.acPACK_MODE = true;
window.acSTARTING_PIECE = "${this.pieceName}"; // Override default "prompt" piece

// Suppress console errors for missing font files in PACK mode
(function() {
  const originalError = console.error;
  console.error = function(...args) {
    const message = args.join(' ');
    // Skip MatrixChunky8 font loading 404 errors
    if (message.includes('Failed to load resource') && 
        (message.includes('MatrixChunky8') || 
         message.includes('assets/type/') ||
         message.includes('.json')) && 
        message.includes('404')) {
      return; // Silently ignore these font loading errors
    }
    originalError.apply(console, args);
  };

  // Capture resource errors at the window level before they hit the console
  window.addEventListener('error', (event) => {
    const target = event?.target;
    const message = event?.message || '';
    const resourceUrl = typeof target?.src === 'string' ? target.src : '';
    if (
      (resourceUrl && resourceUrl.includes('MatrixChunky8')) ||
      message.includes('MatrixChunky8') ||
      message.includes('assets/type/')
    ) {
      event.preventDefault();
      event.stopImmediatePropagation();
      return false;
    }
  }, true);

  window.addEventListener('unhandledrejection', (event) => {
    const reason = event?.reason;
    if (typeof reason === 'string' && reason.includes('MatrixChunky8')) {
      event.preventDefault();
    } else if (reason && typeof reason.message === 'string' && reason.message.includes('MatrixChunky8')) {
      event.preventDefault();
    }
  });
})();

// Colophonic information for provenance and debugging
window.acPACK_COLOPHON = ${JSON.stringify(colophonData, null, 2)};

// Extract Teia URL parameters
const urlParams = new URLSearchParams(window.location.search);
window.acPACK_VIEWER = urlParams.get('viewer') || null;
window.acPACK_CREATOR = urlParams.get('creator') || null;

// Add custom density parameter to URL if specified
${this.options.density ? `
if (!urlParams.has('density')) {
  urlParams.set('density', '${this.options.density}');
  // Update the URL without page reload to include density parameter when environment allows it
  if (window.location && window.location.origin && window.location.origin !== 'null' && window.location.protocol !== 'about:') {
    try {
      const newUrl = window.location.pathname + '?' + urlParams.toString();
      history.replaceState(null, '', newUrl);
    } catch (error) {
      console.warn('⚠️ Skipped history.replaceState in restricted environment:', error?.message || error);
    }
  }
  console.log('🔍 Applied custom density: ${this.options.density}');
}` : '// No custom density specified'}

// Force sandbox mode for Teia
window.acSANDBOX_MODE = true;

// Disable session for pack mode (no need for session in standalone packages)
window.acDISABLE_SESSION = true;

// Enable nogap mode by default for teia
window.acNOGAP_MODE = true;

// Set PACK mode on both window and globalThis for maximum compatibility
window.acPACK_MODE = true;
globalThis.acPACK_MODE = true;

console.log('🎭 PACK mode activated:', {
  startingPiece: window.acSTARTING_PIECE,
  viewer: window.acPACK_VIEWER,
  creator: window.acPACK_CREATOR,
  disableSession: window.acDISABLE_SESSION,
  nogapMode: window.acNOGAP_MODE,
  objktMode: window.acPACK_MODE,
  customDensity: ${this.options.density ? `'${this.options.density}'` : 'null'}
});

// Periodically ensure PACK mode stays enabled
setInterval(() => {
  if (!window.acPACK_MODE || !globalThis.acPACK_MODE) {
    console.log('� Restoring PACK mode flags');
    window.acPACK_MODE = true;
    globalThis.acPACK_MODE = true;
  }
}, 100);
    </script>
  ${this.generateMatrixChunkyGlyphScript()}
  ${this.generateKidLispCacheScript()}
  <script src="${this.getPackagedRelativePath('boot.js')}" type="module" defer onerror="handleModuleLoadError()"></script>
  <link rel="stylesheet" href="${this.getPackagedRelativePath('style.css')}" />
    <style>
      /* Keep transparent background for PACK mode */
      body.nogap {
        background-color: transparent !important;
      }
      
      /* Show console div for file:// protocol errors */
      .file-protocol-error #console {
        display: block !important;
        background: linear-gradient(135deg, #1a1a2e, #16213e);
        color: #ffffff;
        font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
        font-size: 14px;
        line-height: 1.5;
        padding: 30px;
        box-sizing: border-box;
        z-index: 9999;
        border: 2px solid #4a9eff;
        border-radius: 12px;
        margin: 20px;
        max-height: 90vh;
        overflow-y: auto;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3);
      }
      
      .file-protocol-error #console h3 {
        color: #ff6b6b;
        margin-top: 0;
        font-size: 20px;
        text-shadow: 0 2px 4px rgba(0, 0, 0, 0.5);
      }
      
      .file-protocol-error #console h4 {
        color: #4ecdc4;
        font-size: 18px;
        margin-top: 30px;
        margin-bottom: 15px;
        border-bottom: 2px solid #4ecdc4;
        padding-bottom: 8px;
      }
      
      .file-protocol-error #console code {
        background: #2a2a4a;
        color: #ffd93d;
        padding: 3px 6px;
        border-radius: 4px;
        font-family: inherit;
      }
      
      .file-protocol-error #console ul {
        background: rgba(255, 255, 255, 0.05);
        padding: 15px 20px;
        border-radius: 8px;
        border-left: 4px solid #4a9eff;
      }
      
      .source-code-section {
        margin-top: 30px;
        padding: 20px;
        background: rgba(255, 255, 255, 0.03);
        border-radius: 12px;
        border: 1px solid #4ecdc4;
      }
      
      .source-code-container {
        position: relative;
        margin: 15px 0;
      }
      
      .source-code {
        background: #0f0f23;
        color: #a6e22e;
        padding: 20px;
        border-radius: 8px;
        font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
        font-size: 13px;
        line-height: 1.4;
        overflow-x: auto;
        white-space: pre-wrap;
        word-wrap: break-word;
        border: 1px solid #4a9eff;
        user-select: text;
        -webkit-user-select: text;
        -moz-user-select: text;
        -ms-user-select: text;
        max-height: 300px;
        overflow-y: auto;
      }
      
      .copy-button {
        position: absolute;
        top: 10px;
        right: 10px;
        background: #4a9eff;
        color: white;
        border: none;
        padding: 8px 12px;
        border-radius: 6px;
        cursor: pointer;
        font-size: 12px;
        transition: all 0.2s ease;
      }
      
      .copy-button:hover {
        background: #357abd;
        transform: translateY(-1px);
      }
      
      .piece-info {
        background: rgba(0, 0, 0, 0.3);
        padding: 15px;
        border-radius: 8px;
        margin-top: 15px;
        border-left: 4px solid #a6e22e;
      }
      
      .piece-info p {
        margin: 5px 0;
        color: #e0e0e0;
      }
      
      .piece-info strong {
        color: #4ecdc4;
      }
    </style>
  </head>
  <body class="native-cursor">
    <div id="console" class="hidden">
      <div class="boot-message">booting...</div>
      <div class="error-message" style="display: none;">
        <h3>🚫 Module Loading Error (CORS)</h3>
        <p>This OBJKT package needs to be served from an HTTP server to work properly.</p>
        <p><strong>Quick Solutions:</strong></p>
        <ul>
          <li><strong>Python:</strong> <code>python -m http.server 8000</code></li>
          <li><strong>Node.js:</strong> <code>npx serve .</code></li>
          <li><strong>PHP:</strong> <code>php -S localhost:8000</code></li>
        </ul>
        <p>Then open <code>http://localhost:8000</code> in your browser.</p>
        <p><em>Reason: Browsers block ES modules when opened directly as files (file:// protocol) due to CORS security policy.</em></p>
        
        ${colophonData.piece.sourceCode ? `
        <div class="source-code-section">
          <h4>📜 Source Code</h4>
          <div class="source-code-container">
            <pre class="source-code">${colophonData.piece.sourceCode}</pre>
            <button class="copy-button" onclick="copySourceCode()">📋 Copy</button>
          </div>
          <div class="piece-info">
            <p><strong>Piece:</strong> ${colophonData.piece.name}</p>
            <p><strong>Author:</strong> ${colophonData.build.author}</p>
            <p><strong>Created:</strong> ${colophonData.build.packTimeLocal || colophonData.build.packTime} (${colophonData.build.timeZone || "local"})</p>
            <p><strong>Type:</strong> ${colophonData.piece.isKidLisp ? 'KidLisp' : 'JavaScript'} (${colophonData.piece.codeLength} characters)</p>
          </div>
        </div>
        ` : ''}
      </div>
    </div>
    <script>
      if (window.self !== window.top) document.body.classList.add("embed");
      
      // Auto-enable nogap for pack mode or URL parameter
      const params = new URLSearchParams(location.search);
      if (window.acNOGAP_MODE || params.has("nogap") || location.search.includes("nogap")) {
        document.body.classList.add("nogap");
      }
      
      // Handle module loading errors (CORS issues with file:// protocol)
      function handleModuleLoadError() {
        // Skip CORS error overlay if running in Electron
        if (window.navigator.userAgent.includes('Electron')) {
          console.log('🔌 Running in Electron - suppressing CORS error overlay');
          return;
        }
        
        console.error('❌ Failed to load boot.mjs - likely due to CORS policy with file:// protocol');
        document.body.classList.add('file-protocol-error');
        document.querySelector('#console .boot-message').style.display = 'none';
        document.querySelector('#console .error-message').style.display = 'block';
        document.getElementById('console').classList.remove('hidden');
      }
      
      // Also catch any other module loading errors
      window.addEventListener('error', function(e) {
        if (e.message && e.message.includes('Failed to resolve module specifier')) {
          handleModuleLoadError();
        }
      });
      
      // Timeout fallback - if nothing loads after 3 seconds, assume CORS error
      // BUT skip this check if we're running in Electron
      setTimeout(function() {
        if (!window.acBOOTED && location.protocol === 'file:' && !window.navigator.userAgent.includes('Electron')) {
          handleModuleLoadError();
        }
      }, 3000);
    </script>
  </body>
</html>`;

    await fs.writeFile(path.join(this.options.outputDir, "index.html"), indexHtml);
    console.log("📄 Generated index.html");
    
    // Generate a basic manifest.json for the packaged piece
    // Check for 256x256 icon (preferred) or fallback to 128x128
    const icon256Dir = path.join(this.options.outputDir, "icon", "256x256");
    const icon128Dir = path.join(this.options.outputDir, "icon", "128x128");
    const png256Icon = path.join(icon256Dir, `${this.pieceName}.png`);
    const png128Icon = path.join(icon128Dir, `${this.pieceName}.png`);
    const gif128Icon = path.join(icon128Dir, `${this.pieceName}.gif`);
    
    let iconSrc, iconType, iconSize;
    if (fsSync.existsSync(png256Icon)) {
      iconSrc = `./icon/256x256/${this.pieceName}.png`;
      iconType = "image/png";
      iconSize = "256x256";
    } else if (fsSync.existsSync(png128Icon)) {
      iconSrc = `./icon/128x128/${this.pieceName}.png`;
      iconType = "image/png";
      iconSize = "128x128";
    } else if (fsSync.existsSync(gif128Icon)) {
      iconSrc = `./icon/128x128/${this.pieceName}.gif`;
      iconType = "image/gif";
      iconSize = "128x128";
    } else {
      // Fallback to 256x256 PNG (even if it doesn't exist)
      iconSrc = `./icon/256x256/${this.pieceName}.png`;
      iconType = "image/png";
      iconSize = "256x256";
    }

    const manifest = {
      name: finalMetadata.title || this.options.title,
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
        },
        {
          src: `./icon/256x256/${this.pieceName}.png`,
          sizes: "256x256",
          type: "image/png"
        },
        {
          src: `./icon/512x512/${this.pieceName}.png`,
          sizes: "512x512", 
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

  generateMatrixChunkyGlyphScript() {
    if (!this.matrixChunkyGlyphMap || Object.keys(this.matrixChunkyGlyphMap).length === 0) {
      return '';
    }

    const serializedGlyphs = JSON.stringify(this.matrixChunkyGlyphMap);
    return `<script type="text/javascript">
window.acPACK_MATRIX_CHUNKY_GLYPHS = ${serializedGlyphs};
</script>`;
  }

  async bundleSystemFiles() {
  const acOutputDir = this.getPackagedDirPath();
    await fs.mkdir(acOutputDir, { recursive: true });

    const coreFiles = ["boot.mjs", "style.css", "bios.mjs", "lib/parse.mjs"];

    for (const file of coreFiles) {
      const srcPath = path.join(AC_DIR, file);
      const destPath = path.join(acOutputDir, file);
      
      try {
        // Create directory for files in subdirectories
        await fs.mkdir(path.dirname(destPath), { recursive: true });
        
        let content = await fs.readFile(srcPath, "utf8");
        
        // Patch style.css for better nogap support in pack mode
        if (file === 'style.css') {
          content = this.patchStyleCssForObjkt(content);
          console.log(`🔧 Patched style.css for enhanced nogap support`);
        }
        
        // Patch bios.mjs for pack mode - fix webfont URLs
        if (file === 'bios.mjs') {
          content = await this.patchBiosJsForObjkt(content);
          console.log(`🎨 Patched bios.mjs for pack mode`);
        }
        
        // Patch boot.mjs for pack mode - fix dependency URLs
        if (file === 'boot.mjs') {
          content = await this.patchBootJsForObjkt(content);
          console.log(`🎨 Patched boot.mjs for pack mode`);
        }
        
        // Patch parse.mjs for pack mode - handle piece overrides
        if (file === 'lib/parse.mjs') {
          content = await this.patchParseJsForObjkt(content);
          console.log(`🎨 Patched parse.mjs for pack mode`);
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
  const libOutputDir = this.getPackagedDirPath("lib");
    await fs.mkdir(libOutputDir, { recursive: true });

    const libFiles = await fs.readdir(libDir);
    const jsFiles = libFiles.filter(file => file.endsWith(".mjs"));

    console.log(`📚 Found ${jsFiles.length} library files to bundle`);

    for (const libFile of jsFiles) {
      try {
        const srcPath = path.join(libDir, libFile);
        const destPath = path.join(libOutputDir, libFile);
        let content = await fs.readFile(srcPath, "utf8");
        
        // Patch type.mjs for pack mode - prevent API fallback calls
        if (libFile === 'type.mjs') {
          content = this.patchTypeJsForObjkt(content);
          console.log(`🔧 Patched type.mjs for pack mode`);
        }
        
        // Patch kidlisp.mjs for pack mode - reduce verbose logging
        if (libFile === 'kidlisp.mjs') {
          content = this.patchKidLispJsForObjkt(content);
          console.log(`🔧 Patched kidlisp.mjs for pack mode`);
        }
        
        // Patch headers.mjs for pack mode - fix import statements
        if (libFile === 'headers.mjs') {
          content = this.patchHeadersJsForObjkt(content);
          console.log(`🔧 Patched headers.mjs for pack mode`);
        }
        
        // Patch disk.mjs for pack mode - prevent session connections
        if (libFile === 'disk.mjs') {
          content = await this.patchDiskJsForObjkt(content);
          console.log(`🔧 Patched disk.mjs for pack mode`);
        }
        
        // Patch udp.mjs for pack mode - disable networking functionality
        if (libFile === 'udp.mjs') {
          content = this.patchUdpJsForObjkt(content);
          console.log(`🔧 Patched udp.mjs for pack mode`);
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
      { name: "uniforms.js", content: "// Uniform stub for PACK mode\nexport default {};" },
      { name: "vec4.mjs", content: "// Vec4 stub for PACK mode\nexport default {};" },
      { name: "idb.js", content: "// IndexedDB stub for PACK mode" },
      { name: "geckos.io-client.2.3.2.min.js", content: "// Geckos stub for PACK mode\nexport default null;\nmodule.exports = null;" }
    ];

    for (const stub of stubs) {
      const stubPath = path.join(libOutputDir, stub.name);
      await fs.writeFile(stubPath, stub.content);
      console.log(`📝 Created additional stub: ${stub.name}`);
    }
  }

  async bundleSystemsFiles() {
    const systemsDir = path.join(AC_DIR, "systems");
  const systemsOutputDir = this.getPackagedDirPath("systems");
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
  const depOutputDir = this.getPackagedDirPath("dep");
    
    try {
      await fs.mkdir(depOutputDir, { recursive: true });
      const depFiles = await this.getAllFilesRecursively(depDir);
      
      // Analyze piece dependencies to determine what can be excluded
      let exclusionPatterns = [];
      if (this.pieceData) {
        const { DependencyAnalyzer } = await import('./dependency-analyzer.mjs');
        const analyzer = new DependencyAnalyzer();
        
        const pieceCode = this.pieceData.sourceCode || '';
        const pieceSystem = this.pieceData.system || '';
        const analysis = analyzer.analyzePiece(pieceCode, pieceSystem);
        
        console.log(`🔍 Dependency Analysis:`);
        console.log(`   💾 Potential savings: ${analysis.savings.toFixed(1)}MB`);
        console.log(`   🚫 Excluding: ${analysis.exclusions.join(', ')}`);
        console.log(`   ✅ Required: ${analysis.required.join(', ')}`);
        
        exclusionPatterns = analyzer.generateExclusionPatterns(analysis.exclusions);
      }
      
      // Filter files based on exclusion patterns
      const filteredFiles = depFiles.filter(file => {
        const relativePath = path.relative(depDir, file);
        
        // Check if file matches any exclusion pattern
        for (const pattern of exclusionPatterns) {
          const globPattern = pattern.replace(/\*\*/g, '.*').replace(/\*/g, '[^/]*');
          const regex = new RegExp(`^${globPattern}`, 'i');
          if (regex.test(relativePath)) {
            console.log(`🚫 Excluding: ${relativePath} (matched ${pattern})`);
            return false;
          }
        }
        return true;
      });
      
      console.log(`📦 Bundling ${filteredFiles.length} of ${depFiles.length} dependency files (saved ${depFiles.length - filteredFiles.length} files)...`);

      for (const file of filteredFiles) {
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
  const commonOutputDir = this.getPackagedDirPath("disks", "common");

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
  const drawingsOutputDir = this.getPackagedDirPath("disks", "drawings");

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
        this.logVerbose(`🔤 Bundled font_1: ${totalGlyphs} glyph files`);
      } else {
        this.logVerbose("ℹ️ font_1 directory not found, skipping");
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
  const disksOutputDir = this.getPackagedDirPath("disks");
    await fs.mkdir(disksOutputDir, { recursive: true });

    // Handle KidLisp $codes - create a stub piece that jumps to the cached code
    if (this.pieceName.startsWith('$')) {
      const stubContent = `// KidLisp $code stub for PACK mode
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
  const disksOutputDir = this.getPackagedDirPath("disks");
    
    // Create essential disk stubs that might be required
    const stubs = [
      { name: "chat.mjs", content: "// Chat stub for PACK mode\nexport function boot() {}\nexport function paint() {}\nexport function act() {}" }
    ];

    for (const stub of stubs) {
      const stubPath = path.join(disksOutputDir, stub.name);
      await fs.writeFile(stubPath, stub.content);
      console.log(`📝 Created disk stub: ${stub.name}`);
    }
  }

  async createAssets() {
    // Check if we already have a GIF favicon, if not create PNG
    const faviconGifPath = this.getPackagedDirPath("favicon.gif");
    const faviconPngPath = this.getPackagedDirPath("favicon.png");
    
    try {
      await fs.access(faviconGifPath);
      console.log("🎯 Using existing GIF favicon, skipping PNG creation");
    } catch (error) {
      // No GIF favicon exists, create PNG fallback
      const minimalPng = Buffer.from([
        0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00, 0x00, 0x00, 0x0D,
        0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
        0x08, 0x06, 0x00, 0x00, 0x00, 0x1F, 0x15, 0xC4, 0x89, 0x00, 0x00, 0x00,
        0x0B, 0x49, 0x44, 0x41, 0x54, 0x78, 0x9C, 0x63, 0x00, 0x01, 0x00, 0x00,
        0x05, 0x00, 0x01, 0x0D, 0x0A, 0x2D, 0xB4, 0x00, 0x00, 0x00, 0x00, 0x49,
        0x45, 0x4E, 0x44, 0xAE, 0x42, 0x60, 0x82
      ]);
      await fs.writeFile(faviconPngPath, minimalPng);
      console.log("🖼️ Created fallback favicon.png");
    }
  }

  async bundleWebfonts() {
    const webfontsDir = path.join(__dirname, "..", "system", "public", "type", "webfonts");
    const webfontsOutputDir = path.join(this.options.outputDir, "type", "webfonts");

    try {
      await fs.mkdir(webfontsOutputDir, { recursive: true });
      const webfontFiles = await fs.readdir(webfontsDir);

  this.logVerbose("🔤 Bundling webfonts...");

      for (const fontFile of webfontFiles) {
        const srcPath = path.join(webfontsDir, fontFile);
        const destPath = path.join(webfontsOutputDir, fontFile);
        
        if (fontFile.endsWith(".css") || fontFile.endsWith(".woff2") || fontFile.endsWith(".woff") || fontFile.endsWith(".ttf")) {
          const content = await fs.readFile(srcPath);
          await fs.writeFile(destPath, content);
          this.logVerbose(`🔤 Bundled webfont: ${fontFile}`);
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
        
  this.logVerbose(`🔤 Found ${jsonFiles.length} MatrixChunky8 glyph files to bundle`);
        
        let bundledCount = 0;
        const inlineGlyphMap = {};
        for (const glyphFile of jsonFiles) {
          const srcPath = path.join(matrixFontDir, glyphFile);
          
          // Convert 2-digit hex filename to 4-digit hex for consistency with code expectations
          const hexCode = glyphFile.replace('.json', '');
          const paddedHexCode = hexCode.padStart(4, '0').toUpperCase();
          const destFileName = `${paddedHexCode}.json`;
          const destPath = path.join(matrixOutputDir, destFileName);
          
          const content = await fs.readFile(srcPath, "utf8");
          await fs.writeFile(destPath, content);
          try {
            inlineGlyphMap[paddedHexCode] = JSON.parse(content);
          } catch (error) {
            console.warn(`⚠️ Failed to parse glyph ${destFileName} for inline bundle:`, error.message);
          }
          bundledCount++;
        }
        this.matrixChunkyGlyphMap = inlineGlyphMap;
        
  this.logVerbose(`✅ Successfully bundled ${bundledCount} MatrixChunky8 glyph files`);
        
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
        this.logVerbose(`📋 Created font manifest: _manifest.json`);
        
      } catch (matrixError) {
        this.logVerbose("ℹ️ MatrixChunky8 font directory not found, skipping font bundling");
      }
      
    } catch (error) {
      console.error("❌ Error bundling font assets:", error.message);
      console.error("   Make sure the font directory exists at:", fontsDir);
    }
  }

  async prepareTapeSource() {
    if (!this.options.tapePath || this.tapeInfo) {
      return;
    }

    const tapePath = this.options.tapePath;

    try {
      await fs.access(tapePath);
    } catch (error) {
      throw new Error(`Tape file not found: ${tapePath}`);
    }

    console.log(`📼 Using supplied tape archive: ${tapePath}`);

    const { default: JSZip } = await import("jszip");
    const zipBuffer = await fs.readFile(tapePath);
    const zip = await JSZip.loadAsync(zipBuffer);

    let timingData = [];
    const timingEntry = zip.file("timing.json");
    if (timingEntry) {
      try {
        const timingContent = await timingEntry.async("string");
        timingData = JSON.parse(timingContent);
      } catch (error) {
        console.warn("⚠️ Failed to parse timing.json from tape:", error.message);
      }
    } else {
      console.warn("⚠️ No timing.json found in tape archive; falling back to filename ordering");
    }

    let metadata = null;
    const metadataEntry = zip.file("metadata.json");
    if (metadataEntry) {
      try {
        const metadataContent = await metadataEntry.async("string");
        metadata = JSON.parse(metadataContent);
      } catch (error) {
        console.warn("⚠️ Failed to parse metadata.json from tape:", error.message);
      }
    }

    let frameFilenames = [];
    if (Array.isArray(timingData) && timingData.length > 0) {
      frameFilenames = timingData
        .filter((entry) => entry && entry.filename)
        .map((entry) => entry.filename);
    }

    if (frameFilenames.length === 0) {
      frameFilenames = Object.keys(zip.files).filter((name) => name.endsWith(".png"));
      frameFilenames.sort();
    }

    if (frameFilenames.length === 0) {
      throw new Error("Tape archive did not contain any PNG frames");
    }

    const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), "ac-pack-tape-"));
    this.tempDirs.push(tempDir);

    const framePaths = [];
    const frameDurations = [];

    for (let index = 0; index < frameFilenames.length; index++) {
      const filename = frameFilenames[index];
      const entry = zip.file(filename);
      if (!entry) {
        console.warn(`⚠️ Tape frame missing in archive: ${filename}`);
        continue;
      }

      const frameBuffer = await entry.async("nodebuffer");
      const outputPath = path.join(tempDir, path.basename(filename));
      await fs.writeFile(outputPath, frameBuffer);
      framePaths.push(outputPath);

      if (timingData[index]?.duration !== undefined) {
        const durationValue = Number(timingData[index].duration);
        frameDurations.push(Number.isFinite(durationValue) ? durationValue : 0);
      }
    }

    if (framePaths.length === 0) {
      throw new Error("No frame data could be extracted from tape");
    }

    const audioEntries = zip.file(/soundtrack\.(wav|mp3|ogg|flac)$/i) || [];
    if (audioEntries.length > 0) {
      console.log("🎵 Tape includes soundtrack (not automatically bundled to keep package size manageable)");
    }

    let totalDurationMs;
    if (frameDurations.length === framePaths.length) {
      totalDurationMs = frameDurations.reduce((sum, value) => sum + value, 0);
    } else {
      const fallbackFrameDuration = 1000 / 60;
      totalDurationMs = framePaths.length * fallbackFrameDuration;
    }

    const totalDurationSeconds = totalDurationMs / 1000;

    // Override cover duration metadata to reflect the tape recording
    this.options.coverDurationSeconds = totalDurationSeconds;
    this.options.coverFrameCount = framePaths.length;

    this.tapeInfo = {
      tempDir,
      framePaths,
      frameDurations,
      metadata,
      totalDurationMs,
      frameCount: framePaths.length,
      tapeFilename: path.basename(tapePath),
    };

    console.log(
      `📼 Extracted ${framePaths.length} frames from tape (${totalDurationSeconds.toFixed(2)}s)`
    );
    if (metadata?.scale) {
      console.log(
        `📐 Tape metadata: original ${metadata.originalSize?.width || "?"}x${metadata.originalSize?.height || "?"},` +
        ` scaled ${metadata.scaledSize?.width || "?"}x${metadata.scaledSize?.height || "?"}, scale ${metadata.scale}`
      );
    }
  }

  async generateCoverFromTape() {
    if (!this.tapeInfo) {
      throw new Error("Tape information missing; cannot generate cover from tape");
    }

    const { framePaths, frameDurations, metadata, totalDurationMs, tapeFilename } = this.tapeInfo;

    if (!framePaths || framePaths.length === 0) {
      throw new Error("Tape extraction yielded no frames");
    }

    const coverPath = path.join(this.options.outputDir, "cover.gif");

    console.log("🎞️ Generating animated cover from supplied tape...");
    if (tapeFilename) {
      console.log(`� Tape source: ${tapeFilename}`);
    }

    // Create symlinks with sequential numbering for ffmpeg's image2 demuxer
    const tempFramesDir = path.join(this.tapeInfo.tempDir, "frames-seq");
    await fs.mkdir(tempFramesDir, { recursive: true });
    
    // Calculate start frame based on percentage
    const tapeStartPercent = this.options.tapeStartPercent || 0;
    const startFrameIndex = Math.floor((tapeStartPercent / 100) * framePaths.length);
    const selectedFrames = framePaths.slice(startFrameIndex);
    
    if (selectedFrames.length === 0) {
      throw new Error(`No frames remaining after applying start percentage ${tapeStartPercent}%`);
    }
    
    if (tapeStartPercent > 0) {
      console.log(`⏩ Starting from ${tapeStartPercent}% (frame ${startFrameIndex + 1}/${framePaths.length})`);
      console.log(`📊 Using ${selectedFrames.length} frames (${((selectedFrames.length / framePaths.length) * 100).toFixed(1)}% of tape)`);
    }
    
    for (let i = 0; i < selectedFrames.length; i++) {
      const seqPath = path.join(tempFramesDir, `frame${String(i).padStart(6, '0')}.png`);
      await fs.symlink(selectedFrames[i], seqPath);
    }

    // Use 50fps constant framerate for GIF
    const fps = 50;

    // Use ffmpeg to create GIF from PNG sequence using image2 demuxer
    // Scale down to 512x512 and optimize palette for smaller file size
    // Reduce colors to 128 (from default 256) and use bayer dithering for better compression
    await new Promise((resolve, reject) => {
      const inputPattern = path.join(tempFramesDir, "frame%06d.png");
      const ffmpeg = spawn("ffmpeg", [
        "-framerate", String(fps),
        "-i", inputPattern,
        "-vf", `scale=512:512:flags=neighbor,split[s0][s1];[s0]palettegen=max_colors=128:stats_mode=diff[p];[s1][p]paletteuse=dither=bayer:bayer_scale=3`,
        "-loop", "0",
        "-y",
        coverPath
      ], { stdio: ["pipe", "pipe", "pipe"] });

      let stderr = "";
      ffmpeg.stderr.on("data", (data) => {
        stderr += data.toString();
      });

      ffmpeg.on("close", (code) => {
        if (code === 0) {
          resolve();
        } else {
          console.error("❌ FFmpeg stderr:", stderr);
          reject(new Error(`FFmpeg failed with code ${code}`));
        }
      });

      ffmpeg.on("error", (err) => {
        reject(err);
      });
    });

    const externalCoverFilename = `${this.options.author}-${this.pieceName}-${this.zipTimestamp}-cover.gif`;
    const externalCoverPath = path.join(this.options.targetDir, externalCoverFilename);
    await fs.copyFile(coverPath, externalCoverPath);

    console.log("🎞️ Generated animated cover from tape: cover.gif");
    console.log(`�️ External cover created: ${externalCoverFilename}`);

    // Generate Twitter/X-optimized version (15MB max)
    await this.generateTwitterCoverFromTape(tempFramesDir, selectedFrames.length, externalCoverPath);

    // Generate objkt.com-optimized version (2MB max)
    await this.generateObjktCoverFromTape(tempFramesDir, selectedFrames.length, externalCoverPath);

    this.options.coverImage = "cover.gif";
    this.bundledFiles.add(`tape cover frames: ${framePaths.length}`);

    const totalSeconds = totalDurationMs / 1000;
    console.log(`⏱️ Tape duration: ${totalSeconds.toFixed(2)}s`);
  }

  async generateTwitterCoverFromTape(tempFramesDir, frameCount, fullCoverPath) {
    // Generate a Twitter/X-optimized version (15MB max)
    // Twitter recommends 900x900 for square pixel art GIFs for optimal display quality
    // Limit to 15 seconds at 24fps (360 frames) to keep file size manageable
    const twitterCoverFilename = `${this.options.author}-${this.pieceName}-${this.zipTimestamp}-x.gif`;
    const twitterCoverPath = path.join(this.options.targetDir, twitterCoverFilename);

    const targetFps = 24;
    const maxDurationSeconds = 11;  // 11 seconds for 15MB target
    const maxFrames = Math.min(frameCount, targetFps * maxDurationSeconds);
    
    console.log(`🐦 Generating Twitter/X-optimized version (800x800, ${maxFrames} frames @ ${targetFps}fps, 15MB max)...`);

    // Create a temporary directory with only the frames we want for Twitter
    const twitterFramesDir = path.join(path.dirname(tempFramesDir), "frames-twitter");
    await fs.mkdir(twitterFramesDir, { recursive: true });
    
    // Copy/symlink only the first maxFrames frames
    for (let i = 0; i < maxFrames; i++) {
      const sourceFrame = path.join(tempFramesDir, `frame${String(i).padStart(6, '0')}.png`);
      const targetFrame = path.join(twitterFramesDir, `frame${String(i).padStart(6, '0')}.png`);
      await fs.symlink(sourceFrame, targetFrame);
    }

    const fps = 50;
    const inputPattern = path.join(twitterFramesDir, "frame%06d.png");

    await new Promise((resolve, reject) => {
      const ffmpeg = spawn("ffmpeg", [
        "-framerate", String(fps),
        "-i", inputPattern,
        "-frames:v", String(maxFrames),
        // Twitter version: 800x800, 24fps, 16 colors with bayer dithering for best compression
        "-vf", `fps=${targetFps},scale=800:800:flags=neighbor,split[s0][s1];[s0]palettegen=max_colors=16:stats_mode=diff[p];[s1][p]paletteuse=dither=bayer:bayer_scale=5:diff_mode=rectangle`,
        "-loop", "0",
        "-y",
        twitterCoverPath
      ], { stdio: ["pipe", "pipe", "pipe"] });

      let stderr = "";
      ffmpeg.stderr.on("data", (data) => {
        stderr += data.toString();
      });

      ffmpeg.on("close", (code) => {
        if (code === 0) {
          resolve();
        } else {
          console.error("❌ Twitter cover FFmpeg stderr:", stderr);
          reject(new Error(`Twitter cover FFmpeg failed with code ${code}`));
        }
      });

      ffmpeg.on("error", (err) => {
        reject(err);
      });
    });

    // Check file size and log result
    const stats = await fs.stat(twitterCoverPath);
    const sizeMB = (stats.size / (1024 * 1024)).toFixed(2);
    console.log(`🐦 Twitter/X cover created: ${twitterCoverFilename} (${sizeMB} MB)`);
    
    if (stats.size > 15 * 1024 * 1024) {
      console.warn(`⚠️ Warning: Twitter/X cover is ${sizeMB}MB (exceeds 15MB limit)`);
    }
  }

  async generateObjktCoverFromTape(tempFramesDir, frameCount, externalCoverPath) {
    // Generate an objkt.com-optimized version (2MB max)
    // Use frame sampling to create an abbreviated version
    const objktCoverFilename = `${this.options.author}-${this.pieceName}-${this.zipTimestamp}-objkt.gif`;
    const objktCoverPath = path.join(this.options.targetDir, objktCoverFilename);

    console.log(`📦 Generating objkt.com-optimized version (600x600, full color, slideshow-style)...`);

    // Sample only 6 frames evenly distributed across the animation
    const targetFrames = 6;
    const sampleRate = Math.floor(frameCount / targetFrames);
    const sampledFrameCount = Math.min(targetFrames, frameCount);
    
    // Create a temporary directory with only the sampled frames
    const objktFramesDir = path.join(path.dirname(tempFramesDir), "frames-objkt");
    await fs.mkdir(objktFramesDir, { recursive: true });
    
    // Copy/symlink only the sampled frames, evenly distributed
    for (let i = 0; i < sampledFrameCount; i++) {
      const sourceFrameIndex = Math.floor((i * frameCount) / sampledFrameCount);
      const sourceFrame = path.join(tempFramesDir, `frame${String(sourceFrameIndex).padStart(6, '0')}.png`);
      const targetFrame = path.join(objktFramesDir, `frame${String(i).padStart(6, '0')}.png`);
      await fs.symlink(sourceFrame, targetFrame);
    }

    const inputPattern = path.join(objktFramesDir, "frame%06d.png");

    await new Promise((resolve, reject) => {
      const ffmpeg = spawn("ffmpeg", [
        "-framerate", "4", // Input at 4fps (0.25s per frame)
        "-i", inputPattern,
        // objkt version: 600x600, 256 colors (full palette) for slideshow quality
        "-vf", `scale=600:600:flags=neighbor,split[s0][s1];[s0]palettegen=max_colors=256:stats_mode=diff[p];[s1][p]paletteuse=dither=bayer:bayer_scale=2:diff_mode=rectangle`,
        "-loop", "0",
        "-y",
        objktCoverPath
      ], { stdio: ["pipe", "pipe", "pipe"] });

      let stderr = "";
      ffmpeg.stderr.on("data", (data) => {
        stderr += data.toString();
      });

      ffmpeg.on("close", (code) => {
        if (code === 0) {
          resolve();
        } else {
          console.error("❌ objkt cover FFmpeg stderr:", stderr);
          reject(new Error(`objkt cover FFmpeg failed with code ${code}`));
        }
      });

      ffmpeg.on("error", (err) => {
        reject(err);
      });
    });

    // Check file size and log result
    const stats = await fs.stat(objktCoverPath);
    const sizeMB = (stats.size / (1024 * 1024)).toFixed(2);
    console.log(`📦 objkt.com cover created: ${objktCoverFilename} (${sizeMB} MB, ${sampledFrameCount} sampled frames)`);
    
    if (stats.size > 2 * 1024 * 1024) {
      console.warn(`⚠️ Warning: objkt cover is ${sizeMB}MB (exceeds 2MB limit)`);
    }
  }

  async generateFallbackCoverFromTape() {
    if (!this.tapeInfo) {
      throw new Error("Tape information missing; cannot generate fallback cover");
    }

    const { framePaths, totalDurationMs } = this.tapeInfo;
    if (!framePaths || framePaths.length === 0) {
      throw new Error("Tape extraction yielded no frames");
    }

    console.log("🖼️ Generating fallback cover from tape middle frame...");

    const GIFEncoder = await this.getGifEncoderClass();
    const PNG = await this.getPNGClass();

    const coverPath = path.join(this.options.outputDir, "cover.gif");
    const middleIndex = Math.min(framePaths.length - 1, Math.floor(framePaths.length / 2));
    const buffer = await fs.readFile(framePaths[middleIndex]);
    const png = PNG.sync.read(buffer);

    const encoder = new GIFEncoder(png.width, png.height, { highWaterMark: 1 << 24 });
    const writeStream = fsSync.createWriteStream(coverPath);
    encoder.createReadStream().pipe(writeStream);
    encoder.start();
    encoder.setRepeat(0);
    const averageDuration = totalDurationMs && Number.isFinite(totalDurationMs)
      ? totalDurationMs / Math.max(1, framePaths.length)
      : 1000 / 60;
    const fallbackDelay = Math.round(averageDuration) || Math.round(1000 / 60);
    encoder.setDelay(Math.max(16, fallbackDelay));
    encoder.setQuality(10);
    encoder.addFrame(png.data);
    encoder.finish();

    await new Promise((resolve, reject) => {
      writeStream.on("finish", resolve);
      writeStream.on("error", reject);
    });

    const externalCoverFilename = `${this.options.author}-${this.pieceName}-${this.zipTimestamp}-cover.gif`;
    const externalCoverPath = path.join(this.options.targetDir, externalCoverFilename);
    await fs.copyFile(coverPath, externalCoverPath);

    this.options.coverImage = "cover.gif";
    this.bundledFiles.add("tape fallback cover");
    console.log("🖼️ Generated fallback single-frame cover from tape");
  }

  async generateIconsFromTape(iconDirs) {
    if (!this.tapeInfo) {
      throw new Error("Tape information missing; cannot generate icons from tape");
    }

    const { framePaths, frameDurations, totalDurationMs } = this.tapeInfo;
    if (!framePaths || framePaths.length === 0) {
      throw new Error("Tape extraction yielded no frames for icon generation");
    }

    console.log("🖼️ Generating icons from supplied tape...");

    const PNG = await this.getPNGClass();

    const icon128GifPath = path.join(iconDirs.icon128Dir, `${this.pieceName}.gif`);
    const icon128PngPath = path.join(iconDirs.icon128Dir, `${this.pieceName}.png`);
    const icon256PngPath = path.join(iconDirs.icon256Dir, `${this.pieceName}.png`);
    const icon512PngPath = path.join(iconDirs.icon512Dir, `${this.pieceName}.png`);

    // Create symlinks with sequential numbering for ffmpeg's image2 demuxer
    const tempIconFramesDir = path.join(this.tapeInfo.tempDir, "icon-frames-seq");
    await fs.mkdir(tempIconFramesDir, { recursive: true });
    
    for (let i = 0; i < framePaths.length; i++) {
      const seqPath = path.join(tempIconFramesDir, `frame${String(i).padStart(6, '0')}.png`);
      await fs.symlink(framePaths[i], seqPath);
    }

    // Use 50fps constant framerate for icon GIF
    const fps = 50;

    // Generate 128x128 animated GIF using ffmpeg with image2 demuxer
    // Use reduced colors (64) for smaller icon file size
    const inputPattern = path.join(tempIconFramesDir, "frame%06d.png");
    const ffmpegArgs = [
      "-framerate", String(fps),
      "-i", inputPattern,
      "-vf", `scale=128:128:flags=neighbor,split[s0][s1];[s0]palettegen=max_colors=64:stats_mode=diff[p];[s1][p]paletteuse=dither=bayer:bayer_scale=2`,
      "-loop", "0",
      "-y",
      icon128GifPath,
    ];

    await new Promise((resolve, reject) => {
      const ffmpeg = spawn("ffmpeg", ffmpegArgs, { stdio: "pipe" });
      let stderr = "";
      ffmpeg.stderr.on("data", (data) => { stderr += data.toString(); });
      ffmpeg.on("close", (code) => {
        if (code === 0) resolve();
        else reject(new Error(`FFmpeg failed with code ${code}: ${stderr}`));
      });
      ffmpeg.on("error", reject);
    });

    this.bundledFiles.add(`tape icon animation: ${framePaths.length} frames @128x128`);
    this.options.faviconImage = `./icon/128x128/${this.pieceName}.gif`;

    // Generate static PNG icons from final frame (most progressed state)
    const finalIndex = framePaths.length - 1;
    const finalBuffer = await fs.readFile(framePaths[finalIndex]);
    const finalPng = PNG.sync.read(finalBuffer);

    const scaled128Raw = this.scaleImageNearest(finalPng.data, finalPng.width, finalPng.height, 128, 128);
    await this.writePngFromRaw(scaled128Raw, 128, 128, icon128PngPath);
    this.bundledFiles.add(`tape icon static: icon/128x128/${this.pieceName}.png`);

    const scaled256Raw = this.scaleImageNearest(finalPng.data, finalPng.width, finalPng.height, 256, 256);
    await this.writePngFromRaw(scaled256Raw, 256, 256, icon256PngPath);
    this.bundledFiles.add(`tape icon static: icon/256x256/${this.pieceName}.png`);

    const scaled512Raw = this.scaleImageNearest(finalPng.data, finalPng.width, finalPng.height, 512, 512);
    await this.writePngFromRaw(scaled512Raw, 512, 512, icon512PngPath);
    this.bundledFiles.add(`tape icon static: icon/512x512/${this.pieceName}.png`);

    console.log("🖼️ Generated icon set from tape frames");
  }

  async createTransparentFallbackIcons(iconDirs) {
    const icon128Path = path.join(iconDirs.icon128Dir, `${this.pieceName}.png`);
    const icon256Path = path.join(iconDirs.icon256Dir, `${this.pieceName}.png`);
    const icon512Path = path.join(iconDirs.icon512Dir, `${this.pieceName}.png`);

    const transparent128Png = Buffer.from([
      0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
      0x00, 0x00, 0x00, 0x0D,
      0x49, 0x48, 0x44, 0x52,
      0x00, 0x00, 0x00, 0x80,
      0x00, 0x00, 0x00, 0x80,
      0x08, 0x06, 0x00, 0x00, 0x00,
      0xC3, 0x3E, 0x61, 0xCB,
      0x00, 0x00, 0x00, 0x17,
      0x49, 0x44, 0x41, 0x54,
      0x78, 0x9C, 0xED, 0xC1, 0x01, 0x01, 0x00, 0x00, 0x00, 0x80, 0x90, 0xFE,
      0xAF, 0x6E, 0x48, 0x40, 0x00, 0x00, 0x00, 0x02, 0x10, 0x00, 0x01,
      0x8E, 0x0D, 0x71, 0xDA,
      0x00, 0x00, 0x00, 0x00,
      0x49, 0x45, 0x4E, 0x44,
      0xAE, 0x42, 0x60, 0x82
    ]);

    const transparent256Png = Buffer.from([
      0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
      0x00, 0x00, 0x00, 0x0D,
      0x49, 0x48, 0x44, 0x52,
      0x00, 0x00, 0x01, 0x00,
      0x00, 0x00, 0x01, 0x00,
      0x08, 0x06, 0x00, 0x00, 0x00,
      0x5C, 0x72, 0x9C, 0x91,
      0x00, 0x00, 0x00, 0x17,
      0x49, 0x44, 0x41, 0x54,
      0x78, 0x9C, 0xED, 0xC1, 0x01, 0x01, 0x00, 0x00, 0x00, 0x80, 0x90, 0xFE,
      0xAF, 0x6E, 0x48, 0x40, 0x00, 0x00, 0x00, 0x02, 0x10, 0x00, 0x01,
      0x8E, 0x0D, 0x71, 0xDA,
      0x00, 0x00, 0x00, 0x00,
      0x49, 0x45, 0x4E, 0x44,
      0xAE, 0x42, 0x60, 0x82
    ]);

    const transparent512Png = Buffer.from([
      0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
      0x00, 0x00, 0x00, 0x0D,
      0x49, 0x48, 0x44, 0x52,
      0x00, 0x00, 0x02, 0x00,
      0x00, 0x00, 0x02, 0x00,
      0x08, 0x06, 0x00, 0x00, 0x00,
      0xF4, 0x78, 0xD4, 0xFA,
      0x00, 0x00, 0x00, 0x17,
      0x49, 0x44, 0x41, 0x54,
      0x78, 0x9C, 0xED, 0xC1, 0x01, 0x01, 0x00, 0x00, 0x00, 0x80, 0x90, 0xFE,
      0xAF, 0x6E, 0x48, 0x40, 0x00, 0x00, 0x00, 0x02, 0x10, 0x00, 0x01,
      0x8E, 0x0D, 0x71, 0xDA,
      0x00, 0x00, 0x00, 0x00,
      0x49, 0x45, 0x4E, 0x44,
      0xAE, 0x42, 0x60, 0x82
    ]);

    await fs.writeFile(icon128Path, transparent128Png);
    await fs.writeFile(icon256Path, transparent256Png);
    await fs.writeFile(icon512Path, transparent512Png);
    this.logVerbose(`🖼️ Created fallback stub icons: icon/128x128/${this.pieceName}.png, icon/256x256/${this.pieceName}.png and icon/512x512/${this.pieceName}.png`);

    this.options.faviconImage = `./icon/128x128/${this.pieceName}.png`;
  }

  async generateCover() {
    if (this.tapeInfo) {
      try {
        await this.generateCoverFromTape();
        return;
      } catch (error) {
        console.warn("⚠️ Tape-based cover generation failed, falling back to tape fallback cover:", error.message);
        try {
          await this.generateFallbackCoverFromTape();
          return;
        } catch (fallbackError) {
          console.error("❌ Both tape-based cover methods failed. Cannot use orchestrator when --tape is specified:", fallbackError.message);
          throw new Error("Tape-based cover generation failed; orchestrator is disabled when --tape is provided");
        }
      }
    }

    // Try to generate an animated GIF cover using the piece
    try {
      const { RenderOrchestrator } = await import("../reference/tools/recording/orchestrator.mjs");
      const coverPath = path.join(this.options.outputDir, "cover.gif");
      
      // Create a temporary output directory for GIF generation
      const tempOutputDir = path.join(this.options.outputDir, "temp-gif");
      await fs.mkdir(tempOutputDir, { recursive: true });
      
      console.log("🎞️ Generating animated GIF cover...");
      console.log(`📁 Using piece: ${this.pieceName}`);
      console.log(`📁 Base outputDir: ${this.options.outputDir}`);
      console.log(`📁 Temp outputDir: ${tempOutputDir}`);
      
      // Calculate render dimensions based on density
      // Base canvas resolution is 128x128 for crisp pixel art - this is the RECORDING resolution
      // The final GIF will be scaled up by the orchestrator based on density
      const currentDensity = this.options.density || 2; // Default density is 2
      const baseRecordingResolution = 128; // Always record at base resolution
      const coverSeconds = this.options.coverDurationSeconds || 3;
      const frameCount = this.options.coverFrameCount || Math.max(1, Math.round(coverSeconds * 60));
      const coverSecondsDisplay = Number.isInteger(coverSeconds) ? coverSeconds : coverSeconds.toFixed(2);
      
      console.log(`🔍 Density: ${currentDensity}, Recording at: ${baseRecordingResolution}x${baseRecordingResolution}, GIF output will be scaled by orchestrator`);
      console.log(`⏱️ Cover duration: ${coverSecondsDisplay}s (${frameCount} frames @ 60fps)`);
      
      // Use the RenderOrchestrator to generate GIF
      const orchestrator = new RenderOrchestrator(
        this.pieceName,     // piece (supports KidLisp $code or .mjs files)
        frameCount,         // render duration in frames
        tempOutputDir,      // temporary directory for frame rendering
        baseRecordingResolution,   // width - small recording resolution
        baseRecordingResolution,   // height - small recording resolution
        { 
          gifMode: true,    // enable GIF mode
          density: currentDensity, // pass density parameter
          kidlispCache: this.kidlispCacheData, // pass KidLisp cache for dependencies
          extractIconFrame: false, // disable icon extraction - we handle icons separately
          iconOutputDir: this.options.outputDir, // output icon to main directory, not temp
          debugInkColors: this.options.logInkColors,
        }
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
        const externalCoverPath = path.join(this.options.targetDir, externalCoverFilename);
        await fs.copyFile(sourcePath, externalCoverPath);
        
        // Favicon will be set by the icon generation process
        
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
    const coverSvg = `<svg xmlns="http://www.w3.org/2000/svg" width="512" height="512" viewBox="0 0 512 512">
  <rect width="512" height="512" fill="#000"/>
  <text x="256" y="256" text-anchor="middle" dominant-baseline="middle" fill="#fff" font-family="monospace" font-size="20">${this.pieceName}</text>
</svg>`;
    await fs.writeFile(coverPath, coverSvg);
    console.log("🖼️ Generated fallback cover: cover.svg");
    this.options.coverImage = "cover.svg";
    
    // Only use PNG favicon if GIF favicon wasn't already created
    if (!this.options.faviconImage.endsWith('.gif')) {
      this.options.faviconImage = this.getPackagedRelativePath('favicon.png');
    }
  }

  async copyAssets() {
    // Copy cursor files
    try {
      const cursorsSourceDir = path.join(AC_DIR, "cursors");
  const cursorsOutputDir = this.getPackagedDirPath("cursors");
      
      await fs.mkdir(cursorsOutputDir, { recursive: true });
      
      // Copy all cursor files
      const cursorFiles = await fs.readdir(cursorsSourceDir);
      
      for (const file of cursorFiles) {
        if (file.endsWith('.svg')) {
          const sourcePath = path.join(cursorsSourceDir, file);
          const outputPath = path.join(cursorsOutputDir, file);
          await fs.copyFile(sourcePath, outputPath);
          this.logVerbose(`📎 Bundled cursor: ${file}`);
        }
      }
    } catch (error) {
      console.log("ℹ️ Cursor directory error:", error.message);
    }
    
    // Generate icons from tape frames or cover GIF
    const icon128Dir = path.join(this.options.outputDir, "icon", "128x128");
    const icon256Dir = path.join(this.options.outputDir, "icon", "256x256");
    const icon512Dir = path.join(this.options.outputDir, "icon", "512x512");
    await fs.mkdir(icon128Dir, { recursive: true });
    await fs.mkdir(icon256Dir, { recursive: true });
    await fs.mkdir(icon512Dir, { recursive: true });

    const iconDirs = { icon128Dir, icon256Dir, icon512Dir };

    if (this.tapeInfo) {
      try {
        await this.generateIconsFromTape(iconDirs);
      } catch (error) {
        console.warn("⚠️ Tape-based icon generation failed; creating transparent fallbacks:", error.message);
        try {
          await this.createTransparentFallbackIcons(iconDirs);
        } catch (fallbackError) {
          console.log("ℹ️ Error creating fallback icon stubs:", fallbackError.message);
        }
      }
    } else {
      try {
        const coverPath = path.join(this.options.outputDir, "cover.gif");
        if (!fsSync.existsSync(coverPath)) {
          throw new Error("No cover GIF available for stub icon generation");
        }

        this.logVerbose(`🖼️ Generating stub icons for ${this.pieceName} using cover.gif...`);

        await new Promise((resolve, reject) => {
          const ffmpeg = spawn("ffmpeg", [
            "-i", coverPath,
            "-vf", "scale=128:128:flags=neighbor",
            "-y",
            path.join(icon128Dir, `${this.pieceName}.gif`)
          ], { stdio: ["pipe", "pipe", "pipe"] });

          let stderr = "";
          ffmpeg.stderr.on("data", (data) => {
            stderr += data.toString();
          });

          ffmpeg.on("close", (code) => {
            if (code === 0) {
              this.logVerbose(`🪄 Generated 128x128 animated stub icon: icon/128x128/${this.pieceName}.gif`);
              this.options.faviconImage = `./icon/128x128/${this.pieceName}.gif`;
              resolve();
            } else {
              console.warn("⚠️ 128x128 animated stub icon generation failed with code:", code);
              console.warn("⚠️ FFmpeg stderr:", stderr);
              reject(new Error(`FFmpeg failed with code ${code}`));
            }
          });

          ffmpeg.on("error", (err) => {
            console.warn("⚠️ FFmpeg error:", err.message);
            reject(err);
          });
        });

        const staticIconConfigs = [
          { size: 128, output: path.join(icon128Dir, `${this.pieceName}.png`) },
          { size: 256, output: path.join(icon256Dir, `${this.pieceName}.png`) },
          { size: 512, output: path.join(icon512Dir, `${this.pieceName}.png`) }
        ];

        for (const config of staticIconConfigs) {
          await new Promise((resolve, reject) => {
            const ffmpeg = spawn("ffmpeg", [
              "-i", coverPath,
              `-vf`, `scale=${config.size}:${config.size}:flags=neighbor,select=eq(n\\,90)`,
              "-vframes", "1",
              "-y",
              config.output
            ], { stdio: ["pipe", "pipe", "pipe"] });

            let stderr = "";
            ffmpeg.stderr.on("data", (data) => {
              stderr += data.toString();
            });

            ffmpeg.on("close", (code) => {
              if (code === 0) {
                this.logVerbose(`🪄 Generated ${config.size}x${config.size} static PNG icon: ${path.relative(this.options.outputDir, config.output)}`);
                resolve();
              } else {
                console.warn(`⚠️ ${config.size}x${config.size} static PNG icon generation failed with code:`, code);
                console.warn("⚠️ FFmpeg stderr:", stderr);
                reject(new Error(`FFmpeg failed with code ${code}`));
              }
            });

            ffmpeg.on("error", (err) => {
              console.warn("⚠️ FFmpeg error:", err.message);
              reject(err);
            });
          });
        }
      } catch (error) {
        console.log("ℹ️ Stub icon generation failed, creating static PNG fallbacks:", error.message);
        try {
          await this.createTransparentFallbackIcons(iconDirs);
        } catch (fallbackError) {
          console.log("ℹ️ Error creating fallback icon stubs:", fallbackError.message);
        }
      }
    }
    
    console.log("📎 Asset copying complete (minimal set)");
  }

  patchStyleCssForObjkt(content) {
    // Enhance nogap mode for full viewport coverage
    console.log("🔧 Patching style.css for enhanced nogap support...");
    
    // Replace precise.svg cursor with viewpoint.svg for PACK mode
    let patched = content.replace(
      /cursor:\s*url\(['"]?cursors\/precise\.svg['"]?\)\s*12\s*12,\s*auto;/g,
      "cursor: url('cursors/viewpoint.svg') 12 12, auto;"
    );
    
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
}`;    // Replace existing nogap rules or add them if they don't exist
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

  patchTypeJsForObjkt(content) {
    console.log("🔧 Patching type.mjs for pack mode...");
    
    let patched = content;
    
    // Suppress fetch errors for missing MatrixChunky8 font files in PACK mode
    const fetchPattern = /const response = await fetch\(glyphPath\);[\s\S]*?if \(!response\.ok\) \{\s*throw new Error\(`HTTP \$\{response\.status\}: \$\{response\.statusText\}`\);\s*\}/;
    patched = patched.replace(fetchPattern, 
      `const response = await fetch(glyphPath);
            if (!response.ok) {
              // Silently fail for missing font files in PACK mode to avoid console errors
              throw new Error(\`HTTP \${response.status}: \${response.statusText}\`);
            }`
    );
    
    return patched;
  }

  async patchBootJsForObjkt(content) {
    console.log('🎨 Patching boot.mjs for teia dependency URLs and TV mode...');
    
    let patched = content;
    const packagedAuth0Path = this.getPackagedRelativePath('dep', 'auth0-spa-js.production.js');
    
    // Force TV mode (non-interactive) when in PACK mode
    const tvParamPattern = /const tv = tvParam === true \|\| tvParam === "true";/;
    patched = patched.replace(tvParamPattern, 
      `const tv = tvParam === true || tvParam === "true" || window.acPACK_MODE;`
    );
    
    // Fix auth0 script URL to use relative path in pack mode
    const auth0Pattern = /script\.src = "\/aesthetic\.computer\/dep\/auth0-spa-js\.production\.js";/;
    patched = patched.replace(auth0Pattern, 
      `// Check if we're in pack mode for relative path
      const isObjktMode = (typeof window !== 'undefined' && window.acPACK_MODE) ||
                       (typeof globalThis !== 'undefined' && globalThis.acPACK_MODE);
      
      if (isObjktMode) {
        script.src = "${packagedAuth0Path}";
      } else {
        script.src = "/aesthetic.computer/dep/auth0-spa-js.production.js";
      }`
    );
    
    return patched;
  }

  patchKidLispJsForObjkt(content) {
    // kidlisp.mjs now has built-in OBJKT support via getCachedCodeMultiLevel
    // No patching needed anymore
    return content;
  }  async patchDiskJsForObjkt(content) {
    console.log('🔧 Patching disk.mjs for PACK mode...');
    
    let patched = content;
    
    // Add pack mode check to prevent session connections
    const socketPattern = /if \(\s*\/\/parsed\.search\?\.startsWith\("preview"\) \|\|\s*\/\/parsed\.search\?\.startsWith\("icon"\)\s*previewOrIconMode\s*\) \{/;
    patched = patched.replace(socketPattern, 
      `if (
      //parsed.search?.startsWith("preview") ||
      //parsed.search?.startsWith("icon")
      previewOrIconMode ||
      (typeof window !== 'undefined' && window.acPACK_MODE) ||
      (typeof globalThis !== 'undefined' && globalThis.acPACK_MODE)
    ) {`
    );
    
    return patched;
  }

  patchUdpJsForObjkt(content) {
    console.log('🔧 Patching udp.mjs for PACK mode...');
    
    // Replace the entire UDP module with a stub that provides the same API
    // but doesn't try to import geckos or establish network connections
    const teiaStub = `// UDP stub for PACK mode - networking disabled for offline use

const logs = { udp: false }; // Disable UDP logging in PACK mode

let connected = false;

// Stub functions that match the original UDP API but don't do networking
function connect(port = 8889, url = undefined, send) {
  if (logs.udp) console.log("🩰 UDP disabled in PACK mode");
  connected = false; // Always stay disconnected in PACK mode
  return;
}

function disconnect() {
  if (logs.udp) console.log("🩰 UDP disconnect (PACK mode)");
  connected = false;
}

function send(data, options = {}) {
  if (logs.udp) console.log("🩰 UDP send disabled in PACK mode:", data);
  // No-op in PACK mode
}

function isConnected() {
  return false; // Always disconnected in PACK mode
}

// Create UDP object that matches the expected API structure
const UDP = { connect, disconnect, send, isConnected };

// Export the same API as the original UDP module
export { connect, disconnect, send, isConnected, UDP };
export default { connect, disconnect, send, isConnected, UDP };
`;
    
    return teiaStub;
  }

  patchHeadersJsForObjkt(content) {
    console.log('🎨 Patching headers.mjs for teia import statements...');
    
    let patched = content;
    
    // Replace the import statement with stub functions that preserve functionality
    // but don't rely on external module loading
    const stubFunctions = `
// Inlined color-highlighting functions for PACK mode (to avoid import 404s)
function colorizeColorName(colorName) {
  // Simple colorization - just return the color name for now in PACK mode
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
      '// Import replaced with inline functions for PACK mode' + stubFunctions
    );
    
    return patched;
  }

  async patchBiosJsForObjkt(content) {
    console.log('🎨 Patching bios.mjs for teia webfont URLs...');
    
    let patched = content;
    const packagedCursorPath = this.getPackagedRelativePath('cursors', 'viewpoint.svg');
    const packagedDiskLibPath = this.getPackagedRelativePath('lib', 'disk.mjs');
    
    // Replace precise.svg cursor references with viewpoint.svg for PACK mode
    patched = patched.replace(
      /\/aesthetic\.computer\/cursors\/precise\.svg/g,
      packagedCursorPath
    );
    
    // Replace the font URL logic to use relative paths in pack mode
    const fontUrlPattern = /\/\/ Use origin-aware font loading\s*let fontUrl;\s*try \{[\s\S]*?link\.href = fontUrl;/;
    patched = patched.replace(fontUrlPattern, 
      `// Use origin-aware font loading with pack mode support
        let fontUrl;
        try {
          // Check if we're in pack mode
          const isObjktMode = (typeof window !== 'undefined' && window.acPACK_MODE) ||
                           (typeof globalThis !== 'undefined' && globalThis.acPACK_MODE);
          
          if (isObjktMode) {
            // In pack mode, use relative path to bundled webfonts
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
    
    // Also patch the worker path to use relative paths in pack mode
    const workerPathPattern = /const fullPath =\s*"\/aesthetic\.computer\/lib\/disk\.mjs"\s*\+\s*window\.location\.search\s*\+\s*"#"\s*\+\s*Date\.now\(\);/;
    patched = patched.replace(workerPathPattern, 
      `const fullPath = (typeof window !== 'undefined' && window.acPACK_MODE) ? 
        "${packagedDiskLibPath}" + "#" + Date.now() : 
        "/aesthetic.computer/lib/disk.mjs" + window.location.search + "#" + Date.now();`
    );
    
    // Also patch the initial piece parsing to prioritize acSTARTING_PIECE in pack mode
    const pieceParsingPattern = /const parsed = parse\(sluggy \|\| window\.acSTARTING_PIECE\);/;
    patched = patched.replace(pieceParsingPattern, 
      `const parsed = parse((typeof window !== 'undefined' && window.acPACK_MODE && window.acSTARTING_PIECE) ? 
        window.acSTARTING_PIECE : 
        (sluggy || window.acSTARTING_PIECE));`
    );
    
    // Patch the worklet loading to skip in PACK mode to prevent AbortError
    const workletPattern = /\/\/ Sound Synthesis Processor\s*try \{\s*\(async \(\) => \{/;
    patched = patched.replace(workletPattern, 
      `// Sound Synthesis Processor
    try {
      // Skip worklet loading in PACK mode to prevent AbortError
      const isObjktMode = (typeof window !== 'undefined' && window.acPACK_MODE) ||
                        (typeof globalThis !== 'undefined' && globalThis.acPACK_MODE);
      
      if (isObjktMode) {
        if (debug) console.log("🎭 Skipping audio worklet loading in PACK mode");
        return;
      }
      
      (async () => {`
    );
    
    // Patch worker detection to disable workers in sandboxed environments like OBJKT
    const workerDetectionPattern = /\/\/ Override: force disable workers only for specific problematic environments\s*if \(sandboxed && window\.origin === "null" && !window\.acPACK_MODE\) \{\s*\/\/ Only disable for truly sandboxed non-TEIA environments\s*workersEnabled = false;\s*\}/;
    patched = patched.replace(workerDetectionPattern,
      `// Override: force disable workers for OBJKT and other sandboxed environments
  if (sandboxed || window.origin === "null") {
    // Disable workers in any sandboxed environment, including OBJKT
    workersEnabled = false;
    if (debug) console.log("🚫 Workers disabled due to sandboxed/null origin environment");
  }`
    );
    
    // Suppress console errors for missing MatrixChunky8 font files in PACK mode
    const consoleInitPattern = /\/\/ Boot\s*let bootTime/;
    patched = patched.replace(consoleInitPattern, 
      `// Boot - suppress font loading errors in PACK mode
    if (typeof window !== 'undefined' && window.acPACK_MODE) {
      const originalError = console.error;
      console.error = function(...args) {
        const message = args.join(' ');
        // Skip MatrixChunky8 font loading errors
        if (message.includes('Failed to load resource') && 
            message.includes('MatrixChunky8') && 
            message.includes('404')) {
          return; // Silently ignore these errors
        }
        originalError.apply(console, args);
      };
    }
    
    let bootTime`
    );
    
    // Normalize any remaining relative references to the packaged system directory
    patched = patched.replace(/\.\/aesthetic\.computer\//g, `${this.packagedSystemBaseHref}/`);
    
    return patched;
  }

  async patchParseJsForObjkt(content) {
    console.log('🎨 Patching parse.mjs for pack mode piece override...');
    
    let patched = content;
    
    // Override slug function to return acSTARTING_PIECE in pack mode
    const slugFunctionPattern = /function slug\(url\) \{[\s\S]*?return cleanedUrl;\s*\}/;
    patched = patched.replace(slugFunctionPattern, 
      `function slug(url) {
  // In pack mode, always prioritize acSTARTING_PIECE over URL parsing
  if ((typeof window !== 'undefined' && window.acPACK_MODE && window.acSTARTING_PIECE)) {
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

  async convertMjsModulesToJs() {
    const allFiles = await this.getAllFilesRecursively(this.options.outputDir);
    const mjsFiles = allFiles.filter(filePath => filePath.endsWith(".mjs"));

    if (mjsFiles.length === 0) {
      return;
    }

    for (const filePath of mjsFiles) {
      const fileContent = await fs.readFile(filePath, "utf8");
      const updatedContent = this.updateModuleSpecifierExtensions(fileContent);
      const jsPath = filePath.slice(0, -4) + ".js";
      await fs.writeFile(jsPath, updatedContent);
      await fs.rm(filePath);
    }

    const refreshedFiles = await this.getAllFilesRecursively(this.options.outputDir);
    const updateTargets = refreshedFiles.filter(filePath =>
      /(\.js|\.json|\.html|\.css|\.txt|\.svg)$/i.test(filePath)
    );

    for (const filePath of updateTargets) {
      const fileContent = await fs.readFile(filePath, "utf8");
      const updatedContent = this.updateModuleSpecifierExtensions(fileContent);
      if (updatedContent !== fileContent) {
        await fs.writeFile(filePath, updatedContent);
      }
    }

    console.log(`🔁 Converted ${mjsFiles.length} .mjs files to .js for platform compatibility`);
  }

  updateModuleSpecifierExtensions(content) {
    return content.replace(/\.mjs\b/g, ".js");
  }
}

// Main execution
async function main() {
  const pieceName = process.argv[2];
  const args = process.argv.slice(3);
  
  if (!pieceName) {
    console.error("Usage: node ac-pack.mjs <piece-name> [options]");
    console.error("Options:");
    console.error("  --density <value>       Set GIF output density scaling");
    console.error("  --gif-length <seconds>  Set cover GIF length in seconds (default 3)");
    console.error("  --target-dir <path>     Directory where ZIP and cover should be created");
    console.error("  --tape <zip>            Use a pre-recorded tape ZIP instead of rendering live");
    console.error("  --tape-start <percent>  Start cover GIF at this percentage (0-100, default 0)");
    console.error("  --analyze               Show dependency analysis without building");
    console.error("  --verbose               Enable detailed asset/glyph logging");
    console.error("  --quiet                 Suppress detailed asset/glyph logging (default)");
    console.error("  --log-ink               Log resolved colors for ink() and flood() during renders");
    console.error("");
    console.error("Examples:");
    console.error("  node ac-pack.mjs '$bop' --density 8");
    console.error("  node ac-pack.mjs '$bop' --analyze");
    console.error("  node ac-pack.mjs '$bop' --target-dir /path/to/output");
    console.error("  node ac-pack.mjs '$4bb' --tape ./tape.zip");
    console.error("  node ac-pack.mjs '$4bb' --tape ./tape.zip --tape-start 50");
    console.error("");
    console.error("💡 Tip: Run without --tape to be asked if you want to record one first!");
    process.exit(1);
  }

  // Parse command line arguments
  const options = {};
  let analyzeOnly = false;
  let autoShip = false;
  let hasTapeArg = false;
  
  for (let i = 0; i < args.length; i++) {
    if (args[i] === '--density' && i + 1 < args.length) {
      const densityValue = parseFloat(args[i + 1]);
      if (!isNaN(densityValue) && densityValue > 0) {
        options.density = densityValue;
        console.log(`🔍 Custom density: ${densityValue}`);
      } else {
        console.error("❌ Invalid density value. Must be a positive number.");
        process.exit(1);
      }
      i++; // Skip the next argument since we consumed it
    } else if (args[i] === '--gif-length' && i + 1 < args.length) {
      const secondsValue = parseFloat(args[i + 1]);
      if (!isNaN(secondsValue) && secondsValue > 0) {
        options.coverDurationSeconds = secondsValue;
        console.log(`⏱️ Cover GIF length: ${secondsValue}s`);
      } else {
        console.error("❌ Invalid GIF length. Must be a positive number of seconds.");
        process.exit(1);
      }
      i++;
    } else if (args[i] === '--target-dir' && i + 1 < args.length) {
      options.targetDir = args[i + 1];
      console.log(`🎯 Target directory: ${options.targetDir}`);
      i++; // Skip the next argument since we consumed it
    } else if (args[i] === '--tape' && i + 1 < args.length) {
      options.tapePath = args[i + 1];
      hasTapeArg = true;
      console.log(`📼 Tape source provided: ${options.tapePath}`);
      i++;
    } else if (args[i] === '--tape-start' && i + 1 < args.length) {
      options.tapeStartPercent = parseFloat(args[i + 1]);
      if (isNaN(options.tapeStartPercent) || options.tapeStartPercent < 0 || options.tapeStartPercent > 100) {
        console.error("❌ Invalid tape start percentage. Must be between 0 and 100.");
        process.exit(1);
      }
      console.log(`⏩ Tape start position: ${options.tapeStartPercent}%`);
      i++;
    } else if (args[i] === '--analyze') {
      analyzeOnly = true;
    } else if (args[i] === '--auto-ship') {
      autoShip = true;
    } else if (args[i] === '--verbose') {
      options.verbose = true;
    } else if (args[i] === '--quiet') {
      options.verbose = false;
    } else if (args[i] === '--log-ink') {
      options.logInkColors = true;
    }
  }
  
  // Interactive tape recording prompt (only if no --tape provided and not in analyze mode)
  if (!hasTapeArg && !analyzeOnly && process.stdin.isTTY) {
    console.log("");
    console.log("📼 Tape Recording Option");
    console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    console.log("Using a pre-recorded tape gives you:");
    console.log("  • Full control over the animation recording");
    console.log("  • 4 optimized covers: main, Twitter/X, objkt.com, icons");
    console.log("  • Consistent output across all platforms");
    console.log("");
    console.log("To record a tape:");
    console.log(`  1. Visit: https://aesthetic.computer/${pieceName}`);
    console.log("  2. Press 'r' to start recording");
    console.log("  3. Let it animate, then press 'r' again to stop");
    console.log("  4. Download the tape ZIP file");
    console.log("");
    
    const answer = await askQuestion("Would you like to record a tape first? (y/N): ");
    
    if (answer.toLowerCase() === 'y' || answer.toLowerCase() === 'yes') {
      console.log("");
      console.log("📝 Instructions:");
      console.log(`  1. Open: https://aesthetic.computer/${pieceName}`);
      console.log("  2. Press 'r' to start recording");
      console.log("  3. Wait for your animation to complete");
      console.log("  4. Press 'r' again to stop");
      console.log("  5. Download the tape ZIP");
      console.log("  6. Move it to the teia/output/ directory");
      console.log("");
      console.log("When ready, run:");
      console.log(`  node teia/ac-pack.mjs ${pieceName} --tape ./objkt/output/<tape-file>.zip --tape-start 50`);
      console.log("");
      console.log("💡 Tip: Use --tape-start to skip initial loading frames (0-100%)");
      console.log("");
      process.exit(0);
    }
    
    console.log("▶️  Continuing with live rendering (orchestrator mode)...");
    console.log("");
  }
  
  // If analyze-only mode, just show dependency analysis
  if (analyzeOnly) {
    console.log(`🔍 Analyzing dependencies for ${pieceName}...`);
    
    try {
      const tempPacker = new AcPacker(pieceName, options);
      const pieceData = await tempPacker.loadPiece();
      
      const { DependencyAnalyzer } = await import('./dependency-analyzer.mjs');
      const analyzer = new DependencyAnalyzer();
      
      const pieceCode = pieceData.sourceCode || '';
      const pieceSystem = pieceData.system || '';
      const analysis = analyzer.analyzePiece(pieceCode, pieceSystem);
      
      console.log('\n' + analyzer.generateReport(analysis));
      return;
    } catch (error) {
      console.error("❌ Analysis failed:", error.message);
      process.exit(1);
    }
  }

  const packer = new AcPacker(pieceName, options);
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
  console.log("� Package ready for OBJKT deployment!");
  console.log("   • All assets bundled locally");
  console.log("   • Font loading fixed for offline use");
  console.log("   • Session connections disabled");
  console.log("   • PACK mode styling enabled");
  console.log("");
  
  // Automatically create zip with timestamp
  console.log("📦 Creating zip file...");
  try {
    const zipResult = await createZipWithTimestamp(packer.options.outputDir, packer.pieceName, packer.zipTimestamp, packer.options.author, packer.options.targetDir);
    console.log("");
    console.log("🎉 Success! Your package is ready:");
    console.log(`📁 Directory: ${packer.options.outputDir}`);
    console.log(`📦 Zip file: ${zipResult.zipPath}`);
    console.log("");
    
    // Auto-ship to Electron if requested
    if (autoShip) {
      console.log("🚀 Auto-shipping to Electron...");
      try {
        const { ElectronShipper } = await import('./ac-ship.mjs');
        const shipper = new ElectronShipper();
        
        // Use the run method which is the main entry point
        await shipper.run(zipResult.zipPath, ['linux']); // Start with Linux only for testing
        
        console.log("✅ Electron apps built successfully!");
        console.log("📱 Desktop apps ready for distribution");
        console.log("");
        console.log("🚀 Next steps:");
        console.log("1. Test your desktop app");
        console.log("2. Distribute to users");
        console.log("3. Also consider uploading to teia.art or objkt.com for web access");
      } catch (error) {
        console.log("❌ Auto-ship failed:", error.message);
      }
      console.log("");
    } else {
      console.log("🚀 Next steps:");
      console.log("1. Go to https://teia.art/mint");
      console.log(`2. Upload ${path.basename(zipResult.zipPath)}`);
      console.log("3. Preview and test your interactive OBJKT");
      console.log("4. Mint when ready!");
      console.log("");
    }
    
    // Clean up build artifacts (keep only the zip)
    await packer.cleanup();
  } catch (error) {
    console.error("❌ Failed to create zip:", error.message);
    process.exit(1);
  }
}

async function createZipWithTimestamp(outputDir, pieceName, timeStr, author = "@jeffrey", targetDir = null) {
  const archiver = (await import('archiver')).default;
  
  // Use targetDir if provided, otherwise use parent of outputDir
  const zipPath = path.join(targetDir || path.dirname(outputDir), `${author}-${pieceName}-${timeStr}.zip`);
  
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

