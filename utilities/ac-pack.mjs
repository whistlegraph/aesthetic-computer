#!/usr/bin/env node

// ac-pack: Package aesthetic.computer pieces for Teia Interactive OBJKTs
// Usage: node ac-pack.mjs <piece-name> [options]

import { promises as fs } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { createWriteStream } from "fs";
import archiver from "archiver";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Configuration
const SYSTEM_DIR = path.join(__dirname, "..", "system");
const PUBLIC_DIR = path.join(SYSTEM_DIR, "public");
const AC_DIR = path.join(PUBLIC_DIR, "aesthetic.computer");
const DISKS_DIR = path.join(AC_DIR, "disks");
const TOKENS_DIR = path.join(__dirname, "..", "tokens");

class AcPacker {
  constructor(pieceName, options = {}) {
    this.pieceName = pieceName;
    this.options = {
      outputDir: path.join(TOKENS_DIR, pieceName),
      coverImage: options.coverImage || "cover.svg",
      title: options.title || pieceName,
      description: options.description || `Interactive ${pieceName} piece from aesthetic.computer`,
      author: options.author || "aesthetic.computer",
      ...options
    };
    this.bundledFiles = new Set();
  }

  async pack() {
    console.log(`📦 Packing ${this.pieceName} for Teia...`);

    try {
      await this.createOutputDir();
      const pieceData = await this.loadPiece();
      await this.generateIndexHtml(pieceData);
      await this.bundleSystemFiles();
      await this.handleCoverImage();
      await this.copyAssets();
      await this.createZip();

      console.log(`✅ Successfully packed ${this.pieceName} to ${this.options.outputDir}.zip`);
      console.log(`📁 Files bundled: ${this.bundledFiles.size}`);
      
    } catch (error) {
      console.error(`❌ Error packing ${this.pieceName}:`, error);
      throw error;
    }
  }

  async createOutputDir() {
    await fs.mkdir(this.options.outputDir, { recursive: true });
    console.log(`📁 Created output directory: ${this.options.outputDir}`);
  }

  async loadPiece() {
    const piecePath = path.join(DISKS_DIR, this.pieceName);
    let sourceCode, language, metadata = {};

    try {
      sourceCode = await fs.readFile(`${piecePath}.mjs`, "utf8");
      language = "javascript";
    } catch (errJs) {
      try {
        sourceCode = await fs.readFile(`${piecePath}.lisp`, "utf8");
        language = "lisp";
      } catch (errLisp) {
        throw new Error(`Piece '${this.pieceName}' not found. Tried ${piecePath}.mjs and ${piecePath}.lisp`);
      }
    }

    if (language === "javascript") {
      const titleMatch = sourceCode.match(/^\/\/ (.+?)(?:,|\n)/m);
      if (titleMatch) metadata.title = titleMatch[1];
    } else if (language === "lisp") {
      const firstLine = sourceCode.split('\n')[0]?.trim();
      if (firstLine && firstLine.startsWith(';')) {
        metadata.title = firstLine.substring(1).trim();
      }
    }

    console.log(`📜 Loaded ${language} piece: ${this.pieceName}`);
    return { sourceCode, language, metadata };
  }

  async generateIndexHtml(pieceData) {
    const { sourceCode, language, metadata } = pieceData;
    
    const inlinePieceLoader = `
// Teia-compatible piece loader for ${this.pieceName}
window.acTEIA_MODE = true;
window.acPIECE_NAME = "${this.pieceName}";
window.acPIECE_SOURCE = ${JSON.stringify(sourceCode)};
window.acPIECE_LANGUAGE = "${language}";

// Extract Teia URL parameters
const urlParams = new URLSearchParams(window.location.search);
window.acTEIA_VIEWER = urlParams.get('viewer') || null;
window.acTEIA_CREATOR = urlParams.get('creator') || null;

// Force sandbox mode for Teia
window.acSANDBOX_MODE = true;

console.log('🎭 Teia mode activated:', {
  piece: window.acPIECE_NAME,
  viewer: window.acTEIA_VIEWER,
  creator: window.acTEIA_CREATOR
});
`;

    const indexHtml = `<!doctype html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>${metadata.title || this.options.title}</title>
    <meta property="og:image" content="${this.options.coverImage}" />
    <link rel="icon" href="aesthetic.computer/favicon.png" type="image/png" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />
    <meta name="description" content="${this.options.description}" />
    <meta name="og:title" content="${metadata.title || this.options.title}" />
    <meta name="og:description" content="${this.options.description}" />
    <meta name="twitter:card" content="summary_large_image" />
    <meta name="twitter:title" content="${metadata.title || this.options.title}" />
    <meta name="twitter:site" content="${this.options.author}" />
    <meta name="twitter:image" content="${this.options.coverImage}" />
    
    <script type="text/javascript">
${inlinePieceLoader}
    </script>
    
    <script src="aesthetic.computer/boot.mjs" type="module" defer></script>
    <link rel="stylesheet" href="aesthetic.computer/style.css" />
  </head>
  <body class="native-cursor">
    <div id="console" class="hidden">booting...</div>
    <script>
      if (window.self !== window.top) document.body.classList.add("embed");
      const params = new URLSearchParams(location.search);
      if (params.has("nogap") || location.search.includes("nogap")) {
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
        const content = await fs.readFile(srcPath, "utf8");
        let processedContent = content;

        if (file === "boot.mjs") {
          processedContent = await this.modifyBootForTeia(content);
        }

        await fs.writeFile(destPath, processedContent);
        this.bundledFiles.add(file);
        console.log(`📎 Bundled: ${file}`);
      } catch (error) {
        console.warn(`⚠️ Warning: Could not bundle ${file}:`, error.message);
      }
    }

    await this.bundleLibFiles(acOutputDir);
    await this.bundleSystemsFiles(acOutputDir);
    await this.createFavicon(acOutputDir);
  }

  async modifyBootForTeia(bootContent) {
    const teiaInitCode = `
// Teia-specific initialization for sandbox mode
if (window.acTEIA_MODE) {
  console.log('🎭 Initializing in Teia sandbox mode');
  
  // Override piece loading
  const originalLocation = window.location;
  Object.defineProperty(window, 'location', {
    value: {
      ...originalLocation,
      pathname: '/' + window.acPIECE_NAME,
      href: originalLocation.href
    },
    writable: false
  });
  
  // Provide piece source directly
  window.acTEIA_PIECE_SOURCE = window.acPIECE_SOURCE;
}
`;

    let modifiedContent = bootContent.replace(
      'console.clear();',
      'console.clear();\n' + teiaInitCode
    );

    // More precise Auth0 section replacement that preserves syntax
    modifiedContent = modifiedContent.replace(
      /\/\/ #region 🔐 Auth0: Universal Login & Authentication[\s\S]*?\/\/ #endregion/g,
      `// #region 🔐 Auth0: Universal Login & Authentication (DISABLED IN TEIA)
// Authentication completely disabled for Teia sandbox mode
function loadAuth0Script() {
  return Promise.resolve();
}

// Skip auth initialization in sandbox mode
const skipAuth = true;
// #endregion`
    );

    return modifiedContent;
  }

  async bundleLibFiles(acOutputDir) {
    const libDir = path.join(AC_DIR, "lib");
    const libOutputDir = path.join(acOutputDir, "lib");
    
    try {
      await fs.mkdir(libOutputDir, { recursive: true });
      
      const allLibFiles = await fs.readdir(libDir);
      const libMjsFiles = allLibFiles.filter(file => file.endsWith('.mjs'));

      console.log(`📚 Found ${libMjsFiles.length} library files to bundle`);

      for (const libFile of libMjsFiles) {
        try {
          const srcPath = path.join(libDir, libFile);
          const destPath = path.join(libOutputDir, libFile);
          await fs.copyFile(srcPath, destPath);
          this.bundledFiles.add(`lib/${libFile}`);
          console.log(`📚 Bundled lib: ${libFile}`);
        } catch (error) {
          console.warn(`⚠️ Warning: Could not bundle lib/${libFile}:`, error.message);
        }
      }

      // Create stubs for missing files
      const additionalStubs = [
        "sound-whitelist.mjs", "uniforms.js", "vec4.mjs", "idb.js", "geckos.io-client.2.3.2.min.js"
      ];

      for (const stubFile of additionalStubs) {
        const stubPath = path.join(libOutputDir, stubFile);
        const stubContent = `// Stub for ${stubFile} in Teia environment\nexport default {};\n`;
        await fs.writeFile(stubPath, stubContent);
        console.log(`📝 Created additional stub: ${stubFile}`);
      }

    } catch (error) {
      console.warn("⚠️ Warning: Could not create lib directory:", error.message);
    }
  }

  async bundleSystemsFiles(acOutputDir) {
    const systemsDir = path.join(AC_DIR, "systems");
    const systemsOutputDir = path.join(acOutputDir, "systems");
    
    try {
      await fs.mkdir(systemsOutputDir, { recursive: true });
      
      const allSystemFiles = await fs.readdir(systemsDir);
      const systemMjsFiles = allSystemFiles.filter(file => file.endsWith('.mjs'));
      
      console.log(`⚙️ Found ${systemMjsFiles.length} system files to bundle`);
      
      for (const systemFile of systemMjsFiles) {
        try {
          const srcPath = path.join(systemsDir, systemFile);
          const destPath = path.join(systemsOutputDir, systemFile);
          await fs.copyFile(srcPath, destPath);
          this.bundledFiles.add(`systems/${systemFile}`);
          console.log(`⚙️ Bundled system: ${systemFile}`);
        } catch (error) {
          console.warn(`⚠️ Warning: Could not bundle systems/${systemFile}:`, error.message);
        }
      }
    } catch (error) {
      console.warn("⚠️ Warning: Could not bundle systems files:", error.message);
    }
  }

  async createFavicon(acOutputDir) {
    const faviconBase64 = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAAdgAAAHYBTnsmCAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAFYSURBVDiNpZM9SwNBEIafgwQLG1sLwcJCG1sLG0uxsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQsLGwsLBQ";
    
    const faviconBuffer = Buffer.from(faviconBase64, 'base64');
    await fs.writeFile(path.join(acOutputDir, "favicon.png"), faviconBuffer);
    console.log("🖼️ Created favicon.png");
  }

  async handleCoverImage() {
    const coverPath = path.join(this.options.outputDir, this.options.coverImage);
    
    if (this.options.coverImagePath) {
      await fs.copyFile(this.options.coverImagePath, coverPath);
      console.log(`🖼️ Copied cover image: ${this.options.coverImage}`);
    } else {
      await this.generatePlaceholderCover(coverPath);
    }
  }

  async generatePlaceholderCover(coverPath) {
    const svgContent = `<svg width="512" height="512" xmlns="http://www.w3.org/2000/svg">
  <rect width="512" height="512" fill="#1a1a1a"/>
  <text x="256" y="256" text-anchor="middle" dominant-baseline="central" 
        fill="#ffffff" font-family="Arial, sans-serif" font-size="32" font-weight="bold">
    ${this.options.title}
  </text>
  <text x="256" y="300" text-anchor="middle" dominant-baseline="central" 
        fill="#888888" font-family="Arial, sans-serif" font-size="16">
    aesthetic.computer
  </text>
</svg>`;

    const svgPath = coverPath.replace('.png', '.svg');
    await fs.writeFile(svgPath, svgContent);
    
    this.options.coverImage = path.basename(svgPath);
    console.log(`🖼️ Generated placeholder cover: ${this.options.coverImage}`);
  }

  async copyAssets() {
    console.log("📎 Asset copying complete (minimal set)");
  }

  async createZip() {
    const zipPath = `${this.options.outputDir}.zip`;
    const output = createWriteStream(zipPath);
    const archive = archiver('zip', { zlib: { level: 9 } });

    return new Promise((resolve, reject) => {
      output.on('close', () => {
        console.log(`📦 Created zip: ${zipPath} (${archive.pointer()} bytes)`);
        resolve();
      });

      archive.on('error', reject);
      archive.pipe(output);

      archive.directory(this.options.outputDir, false);
      archive.finalize();
    });
  }
}

// CLI interface
async function main() {
  const args = process.argv.slice(2);
  
  if (args.length === 0 || args[0] === '--help' || args[0] === '-h') {
    console.log(`
🎨 ac-pack: Package aesthetic.computer pieces for Teia Interactive OBJKTs

Usage: node ac-pack.mjs <piece-name> [options]

Options:
  --title <title>              Set custom title (default: piece name)
  --description <desc>         Set description  
  --author <author>            Set author (default: aesthetic.computer)
  --cover <path>               Path to custom cover image
  --help                       Show this help

Examples:
  node ac-pack.mjs paint
  node ac-pack.mjs brush --title "My Brush" --author "Artist Name"
  node ac-pack.mjs sparkle --cover ./my-cover.png

Output:
  Creates /tokens/<piece-name>/ directory and <piece-name>.zip file
`);
    return;
  }

  const pieceName = args[0];
  const options = {};

  for (let i = 1; i < args.length; i += 2) {
    const flag = args[i];
    const value = args[i + 1];
    
    switch (flag) {
      case '--title':
        options.title = value;
        break;
      case '--description':
        options.description = value;
        break;
      case '--author':
        options.author = value;
        break;
      case '--cover':
        options.coverImagePath = value;
        break;
    }
  }

  try {
    const packer = new AcPacker(pieceName, options);
    await packer.pack();
  } catch (error) {
    console.error('❌ Packing failed:', error.message);
    process.exit(1);
  }
}

// Check dependencies
try {
  await import('archiver');
} catch (error) {
  console.error('❌ Missing dependency: archiver');
  console.log('📦 Install with: npm install');
  process.exit(1);
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { AcPacker };