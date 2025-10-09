#!/usr/bin/env node

/**
 * Orchestrator Script
 * Manages the stateless frame-by-frame rendering
 * Supports both .mjs pieces and kidlisp $code pieces
 */

import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import https from 'https';
import { timestamp } from '../../../system/public/aesthetic.computer/lib/num.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const ALLOWED_GIF_FPS = [100, 50, 25, 20, 10, 5, 4, 2, 1];

const FRAMES_PER_SECOND = 60;
const DEFAULT_FRAME_COUNT = 300;

class RenderOrchestrator {
  constructor(piece, frames, outputDir, width = 2048, height = 2048, options = {}) {
    this.piece = piece;
    this.frames = frames;
    this.outputDir = outputDir;
    this.width = width;
    this.height = height;
    this.frameRendererPath = path.join(__dirname, 'frame-renderer.mjs');
    this.isKidlispPiece = piece.startsWith('$');
    this.gifMode = options.gifMode || false;
    this.density = options.density || null; // Custom density parameter
    this.kidlispCache = options.kidlispCache || null; // KidLisp dependency cache
    this.extractIconFrame = options.extractIconFrame || false; // Extract midpoint frame as icon
    this.iconOutputDir = options.iconOutputDir || null; // Custom directory for icon output
    this.debugInkColors = options.debugInkColors || false;
    this.gifCompress = options.gifCompress || false; // Ultra-compress GIF for smallest size
    if (this.gifMode) {
      if (options.gifFps && !ALLOWED_GIF_FPS.includes(options.gifFps)) {
        console.warn(`⚠️ Requested GIF fps ${options.gifFps} is not a valid divisor of 100; falling back to nearest supported value.`);
      }
      if (options.gifFps && ALLOWED_GIF_FPS.includes(options.gifFps)) {
        this.gifFps = options.gifFps;
      } else {
        this.gifFps = 50;
      }
      // For GIF mode, render at native GIF fps for smoothest motion
      this.nativeFps = this.gifFps;
    } else {
      this.gifFps = null;
      this.nativeFps = 60; // Standard fps for MP4
    }
  }

  // Fetch KidLisp source code from the localhost API (similar to tape.mjs)
  async fetchKidLispSource(code) {
    const cleanCode = code.replace(/^\$/, '');
    
    // Try localhost first, then fallback to production
    const urls = [
      `https://localhost:8888/.netlify/functions/store-kidlisp?code=${cleanCode}`,
      `https://aesthetic.computer/.netlify/functions/store-kidlisp?code=${cleanCode}`
    ];
    
    for (let i = 0; i < urls.length; i++) {
      const url = urls[i];
      const isLocalhost = url.includes('localhost');
      const serverName = isLocalhost ? 'localhost:8888' : 'aesthetic.computer';
      
      try {
        const source = await this.tryFetchFromUrl(url, isLocalhost);
        if (i > 0) {
          console.log(`🌐 Fetched from ${serverName} (localhost unavailable)`);
        }
        return source;
      } catch (error) {
        if (i === urls.length - 1) {
          // Last attempt failed
          throw new Error(`Could not fetch KidLisp piece '${code}' from any server`);
        }
        // Continue to next server
        if (isLocalhost) {
          console.log(`🔄 localhost:8888 unavailable, trying aesthetic.computer...`);
        }
      }
    }
  }

  // Helper method to try fetching from a specific URL
  async tryFetchFromUrl(url, isLocalhost) {
    return new Promise((resolve, reject) => {
      const options = isLocalhost ? { rejectUnauthorized: false, timeout: 5000 } : { timeout: 10000 }; // 5s for localhost, 10s for production
      
      const req = https.get(url, options, (res) => {
        let data = '';
        res.on('data', (chunk) => data += chunk);
        res.on('end', () => {
          try {
            const response = JSON.parse(data);
            if (response.error) {
              reject(new Error(`KidLisp piece not found`));
              return;
            }
            if (!response.source) {
              reject(new Error("Could not parse source code from response"));
              return;
            }
            resolve(response.source);
          } catch (error) {
            reject(new Error("Could not parse JSON response"));
          }
        });
      }).on('error', (error) => {
        const serverName = isLocalhost ? 'localhost:8888' : 'aesthetic.computer';
        reject(new Error(`Could not connect to ${serverName}`));
      }).on('timeout', () => {
        req.destroy();
        const serverName = isLocalhost ? 'localhost:8888' : 'aesthetic.computer';
        reject(new Error(`Timeout connecting to ${serverName}`));
      });
      
      // Set timeout
      req.setTimeout(isLocalhost ? 5000 : 10000);
    });
  }

  // Extract KidLisp dependencies from source code
  extractDependencies(source) {
    const dependencies = new Set();
    // Match patterns like ($code ...) where code starts with a letter/number
    const matches = source.match(/\(\$[a-zA-Z0-9][a-zA-Z0-9]*\b/g);
    if (matches) {
      for (const match of matches) {
        // Extract just the code part without the opening parenthesis and $
        const code = match.slice(2); // Remove "($"
        dependencies.add(code);
      }
    }
    return Array.from(dependencies);
  }

  // Fetch all dependencies and build a cache
  async fetchDependencies(source) {
    console.log(`🔍 Scanning for dependencies in KidLisp source...`);
    const dependencies = this.extractDependencies(source);
    
    if (dependencies.length === 0) {
      console.log(`📝 No dependencies found`);
      return null;
    }

    console.log(`📦 Found ${dependencies.length} dependencies: ${dependencies.join(', ')}`);
    
    const codesMap = new Map();
    const errors = [];
    
    for (const dep of dependencies) {
      try {
        console.log(`🔄 Fetching dependency: $${dep}`);
        const depSource = await this.fetchKidLispSource(`$${dep}`);
        codesMap.set(dep, { source: depSource });
        console.log(`✅ Cached dependency: $${dep} (${depSource.length} chars)`);
        
        // Recursively fetch dependencies of dependencies
        const subDeps = await this.fetchDependencies(depSource);
        if (subDeps && subDeps.codesMap) {
          for (const [subCode, subData] of subDeps.codesMap) {
            if (!codesMap.has(subCode)) {
              codesMap.set(subCode, subData);
              console.log(`✅ Cached sub-dependency: $${subCode} (${subData.source.length} chars)`);
            }
          }
        }
      } catch (error) {
        console.warn(`⚠️ Failed to fetch dependency $${dep}: ${error.message}`);
        errors.push(`$${dep}: ${error.message}`);
      }
    }

    if (codesMap.size === 0) {
      console.warn(`❌ No dependencies could be fetched. Errors: ${errors.join(', ')}`);
      return null;
    }

    console.log(`🎯 Built dependency cache with ${codesMap.size} codes`);
    return { codesMap };
  }

  // Create a temporary kidlisp piece file for rendering with dependencies
  async createKidlispPieceFile(source) {
    const pieceName = this.piece.replace('$', '');
    const pieceFilePath = path.resolve(this.outputDir, `${pieceName}-kidlisp.mjs`);
    
    try {
      // Generate cache code if we have cached dependencies
      let cacheScript = '';
      if (this.kidlispCache && this.kidlispCache.codesMap) {
        console.log(`🎯 Using provided KidLisp cache with ${this.kidlispCache.codesMap.size} codes`);
        const { generateCacheCode } = await import('../../../objkt/kidlisp-extractor.mjs');
        cacheScript = generateCacheCode(this.kidlispCache.codesMap);
      } else {
        console.log(`⚠️ No KidLisp cache provided - dependencies may not render correctly`);
      }
      
      // Create a piece file that uses the kidlisp() function with cache setup
      const pieceContent = `
// Auto-generated kidlisp piece for recording: ${this.piece}
// Uses built-in kidlisp() function with dependency cache

${cacheScript ? `
// Initialize KidLisp cache at module level (before any KidLisp instances are created)
${cacheScript}
` : '// No KidLisp cache available'}

export async function boot({ screen }) {
  console.log("🎨 Booting kidlisp piece: ${this.piece}");
}

export async function paint(api) {
  try {
    // Use the built-in kidlisp() function with the actual source code
    // Note: Width and height will be auto-detected by the kidlisp function
    const source = \`${source.replace(/\\/g, '\\\\').replace(/`/g, '\\`').replace(/\$/g, '\\$')}\`;
    api.kidlisp(0, 0, undefined, undefined, source);
  } catch (error) {
    console.error("❌ KidLisp execution error:", error);
    // Show error visually
    if (api.wipe && api.ink && api.write) {
      api.wipe('red');
      api.ink('white');
      api.write(\`Error: \${error.message}\`, 10, 10);
    } else {
      console.error("❌ Cannot show error visually - missing API functions");
    }
  }
}
`;
    
      fs.writeFileSync(pieceFilePath, pieceContent);
      console.log(`📝 Created temporary kidlisp piece: ${pieceFilePath}`);
      return pieceFilePath;
    } catch (error) {
      console.error(`❌ Failed to resolve KidLisp dependencies: ${error.message}`);
      // Fallback to simple version without dependencies
      const pieceContent = `
// Auto-generated kidlisp piece for recording: ${this.piece}
// Simple fallback without dependency resolution

// No KidLisp cache available

export async function boot({ screen }) {
  console.log("🎨 Booting kidlisp piece: ${this.piece}");
}

export async function paint(api) {
  try {
    const source = \`${source.replace(/\\/g, '\\\\').replace(/`/g, '\\`').replace(/\$/g, '\\$')}\`;
    api.kidlisp(0, 0, undefined, undefined, source);
  } catch (error) {
    console.error("❌ KidLisp execution error:", error);
    if (api.wipe && api.ink && api.write) {
      api.wipe('red');
      api.ink('white');
      api.write(\`Error: \${error.message}\`, 10, 10);
    }
  }
}
`;
      fs.writeFileSync(pieceFilePath, pieceContent);
      console.log(`📝 Created fallback kidlisp piece: ${pieceFilePath}`);
      return pieceFilePath;
    }
  }

  async renderAll() {
  console.log(`🎬 Starting stateless render: ${this.piece} for ${this.frames} frames`);
    console.log(`📁 Output: ${this.outputDir}`);
    
    let actualPiece = this.piece;
    
    // Handle kidlisp pieces
    if (this.isKidlispPiece) {
      console.log(`🎨 Detected kidlisp piece: ${this.piece}`);
      try {
        const source = await this.fetchKidLispSource(this.piece);
        console.log(`📄 Fetched kidlisp source (${source.length} chars)`);
        
        // Fetch dependencies and build cache if not already provided
        if (!this.kidlispCache) {
          this.kidlispCache = await this.fetchDependencies(source);
        }
        
        actualPiece = await this.createKidlispPieceFile(source);
      } catch (error) {
        console.error(`❌ Failed to fetch kidlisp source: ${error.message}`);
        return;
      }
    }
    
    const startTime = Date.now();
    let frameCount = 0;
    let complete = false;
    let consecutiveFailures = 0;
    const maxRetries = 3;
    
    // Clear screen and hide cursor for progress bar
    process.stdout.write('\x1b[2J\x1b[H\x1b[?25l');
    
    while (!complete && consecutiveFailures < maxRetries) {
      try {
        // Run one frame in fresh process
        // Use single quotes to prevent shell variable expansion of $ceo, etc.
        const densityArg = this.density ? ` ${this.density}` : '';
        const fpsArg = ` ${this.nativeFps}`;
        const command = `node ${this.frameRendererPath} '${actualPiece}' ${this.frames} '${this.outputDir}' ${this.width} ${this.height}${densityArg}${fpsArg}`;
        console.log(`🔧 Executing: ${command}`);
        const env = { ...process.env };
        if (this.debugInkColors) {
          env.AC_LOG_INK_COLORS = '1';
          env.AC_LOG_INK_LABEL = this.piece;
        }
        const result = execSync(
          command,
          { encoding: 'utf8', stdio: 'pipe', env }
        );
        
        // Show frame renderer output in a controlled way
        if (result && result.length > 0) {
          // Only show important messages, filter out verbose output
          const lines = result.split('\n');
          const importantLines = lines.filter(line => 
            line.includes('FRAME') || 
            line.includes('ERROR') || 
            line.includes('WARNING') ||
            line.includes('✅') || 
            line.includes('❌')
          );
          if (importantLines.length > 0) {
            console.log(importantLines.join('\n'));
          }
        }
        
        frameCount++;
        consecutiveFailures = 0; // Reset on success
        
        // Check if complete by looking for state
        const stateFile = path.join(this.outputDir, 'state.json');
        if (fs.existsSync(stateFile)) {
          const state = JSON.parse(fs.readFileSync(stateFile, 'utf8'));
          complete = state.frameIndex >= state.totalFrames;
          
          // Display current frame as full-screen sixel
          await this.displayLastFrameAsSixel();
          
          // Update static progress bar
          this.updateProgressBar(state.frameIndex, state.totalFrames, startTime);
        }
        
      } catch (error) {
        consecutiveFailures++;
        console.error(`💥 Frame ${frameCount} failed (${consecutiveFailures}/${maxRetries}):`, error.message);
        
        if (consecutiveFailures >= maxRetries) {
          console.error(`🚫 Too many consecutive failures. Aborting render.`);
          return;
        }
      }
    }
    
    if (complete) {
      console.log(`🎯 Render complete! ${frameCount} frames rendered with zero memory leaks.`);
      
      // Convert to requested format
      if (this.gifMode) {
        console.log(`�️  Target GIF frame rate: ${this.gifFps}fps (quantized to 10ms steps)`);
        console.log(`�🎬 Converting frames to GIF...`);
        await this.convertToGIF();
      } else {
        console.log(`🎬 Converting frames to MP4...`);
        await this.convertToMP4();
      }
      
      // Cleanup temporary kidlisp piece file if it was created
      if (this.isKidlispPiece && actualPiece !== this.piece) {
        try {
          fs.unlinkSync(actualPiece);
          console.log(`🗑️ Cleaned up temporary kidlisp piece file`);
        } catch (error) {
          console.warn(`⚠️ Could not cleanup temporary file: ${error.message}`);
        }
      }
    }
  }

  async convertToMP4() {
    console.log(`🎬 Converting frames to MP4...`);
    
    try {
      // First concatenate all frame files into one RGB file
      console.log(`📦 Concatenating frame files...`);
      const allFramesFile = path.join(this.outputDir, 'all-frames.rgb');
      execSync(`cat ${this.outputDir}/frame-*.rgb > ${allFramesFile}`, { stdio: 'inherit' });
      
      // Generate proper filename using AC naming scheme
      const pieceName = this.isKidlispPiece ? this.piece.replace('$', '') : path.basename(this.piece, '.mjs');
      const timestampStr = timestamp();
      const durationSeconds = Math.round(this.frames / this.nativeFps * 10) / 10; // Convert frames to seconds at native fps, round to 1 decimal place
      const filename = `${pieceName}-${timestampStr}-${durationSeconds}s.mp4`;
      // Save MP4 one level up from the artifacts directory for better organization
      const outputMP4 = path.join(path.dirname(this.outputDir), filename);
      
      // Get frame dimensions from state file for proper resolution
      const stateFile = path.join(this.outputDir, 'state.json');
      let width = 2048, height = 2048; // defaults
      if (fs.existsSync(stateFile)) {
        const state = JSON.parse(fs.readFileSync(stateFile, 'utf8'));
        width = state.width || 2048;
        height = state.height || 2048;
      }

      const renderWidth = width;
      const renderHeight = height;

      let targetWidth = this.width || renderWidth;
      let targetHeight = this.height || renderHeight;

      if (this.density) {
        targetWidth = Math.max(1, Math.round(targetWidth * this.density));
        targetHeight = Math.max(1, Math.round(targetHeight * this.density));
      }

      let vfFilter = '';
      if (this.density) {
        const baseWidth = Math.max(1, Math.round(targetWidth / this.density));
        const baseHeight = Math.max(1, Math.round(targetHeight / this.density));
        vfFilter = ` -vf "scale=${baseWidth}:${baseHeight}:flags=neighbor,scale=${targetWidth}:${targetHeight}:flags=neighbor"`;
        console.log(`🔍 Density ${this.density}: Rendering at ${renderWidth}x${renderHeight} → chunky ${baseWidth}x${baseHeight} → final ${targetWidth}x${targetHeight} (${this.density}x pixel chunks)`);
      } else if (targetWidth !== renderWidth || targetHeight !== renderHeight) {
        vfFilter = ` -vf "scale=${targetWidth}:${targetHeight}:flags=lanczos"`;
        console.log(`🔍 Scaling MP4: ${renderWidth}x${renderHeight} → ${targetWidth}x${targetHeight} with smooth interpolation`);
      } else {
        console.log(`🔍 MP4 at native resolution ${renderWidth}x${renderHeight}`);
      }
      
      execSync(
        `ffmpeg -y -f rawvideo -pix_fmt rgb24 -s ${renderWidth}x${renderHeight} -r ${this.nativeFps} ` +
        `-i ${allFramesFile} -c:v libx264 -pix_fmt yuv420p` +
        `${vfFilter} -movflags +faststart -r ${this.nativeFps} -b:v 32M ${outputMP4}`,
        { stdio: 'inherit' }
      );
      
      console.log(`✅ MP4 created: ${filename}`);
      
    } catch (error) {
      console.error(`💥 MP4 conversion failed:`, error.message);
    }
  }

  async convertToGIF() {
    console.log(`🎞️ Converting frames to GIF with ffmpeg Floyd-Steinberg dithering...`);
    
    try {
      // Get frame dimensions from state file
      const stateFile = path.join(this.outputDir, 'state.json');
      let width = 2048, height = 2048; // defaults
      let sourceFps = this.nativeFps; // Use the actual rendering fps
      if (fs.existsSync(stateFile)) {
        const state = JSON.parse(fs.readFileSync(stateFile, 'utf8'));
        width = state.width || 2048;
        height = state.height || 2048;
        sourceFps = state.fps || this.nativeFps;
      }

      const renderWidth = width;
      const renderHeight = height;

      let targetWidth = this.width || renderWidth;
      let targetHeight = this.height || renderHeight;

      if (this.density) {
        targetWidth = Math.max(1, Math.round(targetWidth * this.density));
        targetHeight = Math.max(1, Math.round(targetHeight * this.density));
      }

      // Get all RGB frame files
      const rgbFiles = fs.readdirSync(this.outputDir)
        .filter(file => file.endsWith('.rgb'))
        .sort((a, b) => {
          const aFrame = parseInt(a.match(/frame-(\d+)\.rgb/)[1]);
          const bFrame = parseInt(b.match(/frame-(\d+)\.rgb/)[1]);
          return aFrame - bFrame;
        });

      if (rgbFiles.length === 0) {
        throw new Error("No RGB frames found for conversion");
      }

      console.log(`🎨 Converting ${rgbFiles.length} RGB frames to PNG for ffmpeg...`);
      
      // Create temporary directory for PNG frames
      const tempDir = path.join(this.outputDir, 'png_frames');
      if (!fs.existsSync(tempDir)) {
        fs.mkdirSync(tempDir);
      }

      // Convert RGB files to PNG files using ffmpeg
      for (let i = 0; i < rgbFiles.length; i++) {
        const rgbFile = path.join(this.outputDir, rgbFiles[i]);
        const frameNumber = String(i).padStart(6, '0');
        const pngFile = path.join(tempDir, `frame_${frameNumber}.png`);
        
        // Use ffmpeg to convert RGB to PNG directly
        const ffmpegCmd = `ffmpeg -f rawvideo -pix_fmt rgb24 -s ${width}x${height} -i "${rgbFile}" -y "${pngFile}" 2>/dev/null`;
        execSync(ffmpegCmd);

        // Progress indicator
        if (i % 10 === 0) {
          process.stdout.write(`\r🖼️ Converting frame ${i + 1}/${rgbFiles.length} to PNG`);
        }
      }

      console.log(`\n🎬 Creating GIF with ffmpeg Floyd-Steinberg dithering...`);

      // Generate proper filename
      const pieceName = this.isKidlispPiece ? this.piece.replace('$', '') : path.basename(this.piece, '.mjs');
      const timestampStr = timestamp();
      const targetFps = this.gifFps || 50;
      const durationSeconds = Math.round((rgbFiles.length / sourceFps) * 10) / 10;
      const filename = `${pieceName}-${timestampStr}-${durationSeconds}s.gif`;
      const outputGIF = path.join(path.dirname(this.outputDir), filename);

      console.log(`🎬 Native rendering: ${rgbFiles.length} frames at ${sourceFps}fps → direct GIF conversion (no resampling)`);
      
      // Create optimal palette with compression-focused settings
      if (this.gifCompress) {
        console.log(`🗜️ Generating ultra-compressed palette with smart optimization...`);
      } else {
        console.log(`🎨 Generating optimized palette for maximum compression...`);
      }
      // Use fewer colors for compress mode, but with better stats analysis
      const maxColors = this.gifCompress ? 96 : 128;
      const statsMode = this.gifCompress ? "full" : "diff";
      const paletteCmd = `ffmpeg -y -framerate ${sourceFps} -i "${tempDir}/frame_%06d.png" -vf "scale=-1:-1:flags=lanczos,palettegen=reserve_transparent=0:max_colors=${maxColors}:stats_mode=${statsMode}" "${tempDir}/palette.png" 2>/dev/null`;
      execSync(paletteCmd);

      // Create final GIF with optimized compression settings
      if (this.gifCompress) {
        console.log(`🗜️ Applying ULTRA compression (optimized palette + Floyd-Steinberg)...`);
      } else {
        console.log(`🗜️ Applying optimized compression and dithering...`);
      }
      
      let scaleFilter = `scale=${targetWidth}:${targetHeight}:flags=lanczos`;

      if (this.density) {
        // Calculate the render resolution based on density to create chunky pixels before scaling back up
        const baseWidth = Math.max(1, Math.round(targetWidth / this.density));
        const baseHeight = Math.max(1, Math.round(targetHeight / this.density));
        scaleFilter = `scale=${baseWidth}:${baseHeight}:flags=neighbor,scale=${targetWidth}:${targetHeight}:flags=neighbor`;
        console.log(`🔍 Density ${this.density}: Rendering at ${renderWidth}x${renderHeight} → chunky ${baseWidth}x${baseHeight} → final ${targetWidth}x${targetHeight} (${this.density}x pixel chunks)`);
      } else if (targetWidth !== renderWidth || targetHeight !== renderHeight) {
        console.log(`🔍 No density specified: Scaling ${renderWidth}x${renderHeight} → ${targetWidth}x${targetHeight} with smooth interpolation`);
      } else {
        console.log(`🔍 No scaling applied: keeping native resolution ${renderWidth}x${renderHeight}`);
      }
      
      // Floyd-Steinberg usually compresses better than no dithering or bayer
      const ditheringOptions = this.gifCompress 
        ? "paletteuse=dither=floyd_steinberg" 
        : "paletteuse=dither=floyd_steinberg:bayer_scale=3";
      
  const gifCmd = `ffmpeg -y -framerate ${sourceFps} -i "${tempDir}/frame_%06d.png" -i "${tempDir}/palette.png" -lavfi "${scaleFilter}[x];[x][1:v]${ditheringOptions}" "${outputGIF}" 2>/dev/null`;
      execSync(gifCmd);

      // Extract icon frame if requested
      if (this.extractIconFrame) {
        const iconOutputDir = this.iconOutputDir || this.outputDir;
        await this.extractMidpointFrameAsIcon(tempDir, iconOutputDir);
      }

      // Clean up temporary files
      console.log(`🧹 Cleaning up temporary files...`);
      execSync(`rm -rf "${tempDir}"`);

      // Get final file size and log success
      const stats = fs.statSync(outputGIF);
      const fileSizeKB = Math.round(stats.size / 1024);
      console.log(`✅ GIF created: ${filename}`);
      console.log(`📊 File size: ${fileSizeKB}KB`);
      if (this.gifCompress) {
        console.log(`🗜️ Ultra-compressed GIF with minimal file size!`);
      } else {
        console.log(`🗜️ Optimized GIF with maximum compression!`);
      }
      
    } catch (error) {
      console.error(`💥 GIF conversion failed:`, error.message);
      throw error;
    }
  }

  async extractMidpointFrameAsIcon(tempDir, outputDir) {
    try {
      console.log(`🎨 Extracting midpoint frame as icon...`);
      
      // Find all PNG files in temp directory
      const pngFiles = fs.readdirSync(tempDir)
        .filter(file => file.endsWith('.png') && file.startsWith('frame_'))
        .sort((a, b) => {
          const aFrame = parseInt(a.match(/frame_(\d+)\.png/)[1]);
          const bFrame = parseInt(b.match(/frame_(\d+)\.png/)[1]);
          return aFrame - bFrame;
        });

      if (pngFiles.length === 0) {
        console.warn(`⚠️ No PNG frames found for icon extraction`);
        return;
      }

      // Get midpoint frame
      const midpointIndex = Math.floor(pngFiles.length / 2);
      const midpointFrame = pngFiles[midpointIndex];
      const sourcePath = path.join(tempDir, midpointFrame);
      
      // Create icon directories for different sizes
      // Get piece name for icon filename
      const pieceName = this.isKidlispPiece ? this.piece.replace('$', '') : path.basename(this.piece, '.mjs');
      
      // Create both 256x256 and 512x512 versions for better compatibility
      const iconDir256 = path.join(outputDir, 'icon', '256x256');
      const iconDir512 = path.join(outputDir, 'icon', '512x512');
      
      if (!fs.existsSync(iconDir256)) {
        fs.mkdirSync(iconDir256, { recursive: true });
      }
      if (!fs.existsSync(iconDir512)) {
        fs.mkdirSync(iconDir512, { recursive: true });
      }
      
      // Create 256x256 PNG using piece name
      const iconPath256 = path.join(iconDir256, `${pieceName}.png`);
      const iconPath512 = path.join(iconDir512, `${pieceName}.png`);
      
      try {
        // Create 256x256 version
        const ffmpegCmd256 = `ffmpeg -i "${sourcePath}" -vf "scale=256:256:flags=neighbor" -y "${iconPath256}" 2>/dev/null`;
        execSync(ffmpegCmd256);
        
        // Create 512x512 version
        const ffmpegCmd512 = `ffmpeg -i "${sourcePath}" -vf "scale=512:512:flags=neighbor" -y "${iconPath512}" 2>/dev/null`;
        execSync(ffmpegCmd512);
        
        console.log(`✅ Icon extracted from frame ${midpointIndex + 1}/${pngFiles.length}: icon/256x256/${pieceName}.png and icon/512x512/${pieceName}.png created`);
      } catch (error) {
        // Fallback: copy original and create basic versions
        fs.copyFileSync(sourcePath, iconPath256);
        fs.copyFileSync(sourcePath, iconPath512);
        console.log(`✅ Icon extracted from frame ${midpointIndex + 1}/${pngFiles.length}: icon/256x256/${pieceName}.png and icon/512x512/${pieceName}.png created (fallback)`);
      }
      
    } catch (error) {
      console.warn(`⚠️ Could not extract icon frame: ${error.message}`);
    }
  }

  updateProgressBar(currentFrame, totalFrames, startTime) {
    const progress = currentFrame / totalFrames;
    const barWidth = 20;
    const filled = Math.floor(progress * barWidth);
    const empty = barWidth - filled;
    
    const bar = '█'.repeat(filled) + '░'.repeat(empty);
    const percentage = (progress * 100).toFixed(1);
    
    // Calculate elapsed time and estimated remaining time
    const elapsed = Date.now() - startTime;
    const estimatedTotal = elapsed / progress;
    const remaining = estimatedTotal - elapsed;
    
    const formatTime = (ms) => {
      const seconds = Math.floor(ms / 1000);
      const minutes = Math.floor(seconds / 60);
      const hours = Math.floor(minutes / 60);
      
      if (hours > 0) return `${hours}h ${minutes % 60}m ${seconds % 60}s`;
      if (minutes > 0) return `${minutes}m ${seconds % 60}s`;
      return `${seconds}s`;
    };
    
    // Clear line and print progress
    process.stdout.write('\r');
    process.stdout.write(`🎬 ${bar} ${percentage}% (${currentFrame}/${totalFrames} frames) `);
    process.stdout.write(`⏱️ ${formatTime(elapsed)} elapsed`);
    if (progress > 0.01) { // Only show ETA after 1% progress
      process.stdout.write(` | ETA: ${formatTime(remaining)}`);
    }
  }

  // Display last frame as sixel image inline in terminal
  async displayLastFrameAsSixel() {
    try {
      // Find the last individual RGB frame file (exclude all-frames.rgb)
      const rgbFiles = fs.readdirSync(this.outputDir)
        .filter(f => f.endsWith('.rgb') && f.startsWith('frame-'))
        .sort();
      
      if (rgbFiles.length === 0) {
        return; // No frames yet, skip silently
      }
      
      const lastFrame = rgbFiles[rgbFiles.length - 1];
      const framePath = path.join(this.outputDir, lastFrame);
      
      // Read the RGB data
      const rgbData = fs.readFileSync(framePath);
      
      // Get terminal dimensions
      const termWidth = process.stdout.columns || 80;
      const termHeight = process.stdout.rows || 24;
      
      // Calculate optimal size to fill most of the terminal
      // Use very aggressive scaling for large, visually prominent images
      const maxWidth = Math.floor(termWidth * 2.5); // Even wider (sixel pixels are very narrow)
      const maxHeight = Math.floor(termHeight * 4.0); // Much taller for visual impact
      
      // Generate and display sixel at large terminal size
      const sixelData = this.generateSixel(rgbData, this.width, this.height, maxWidth, maxHeight);
      
      // Calculate actual sixel display dimensions (we want it large!)
      const aspectRatio = this.height / this.width;
      const displayWidth = maxWidth; // Use the full available width
      const displayHeight = Math.floor(displayWidth * aspectRatio);
      const sixelHeight = Math.ceil(displayHeight / 6); // Sixel is 6 pixels per line
      
      // Enter alternative screen buffer for clean full-screen display
      process.stdout.write('\x1b[?1049h'); // Enter alt screen buffer
      process.stdout.write('\x1b[2J\x1b[H'); // Clear screen and move to top-left
      
      // Calculate optical centering
      const imageHeightInLines = Math.ceil(displayHeight / 6); // Sixel uses 6-pixel bands
      const verticalPadding = Math.max(1, Math.floor((termHeight - imageHeightInLines) / 2));
      
      // Better horizontal centering calculation
      // Sixel pixels are about 1/2 the width of a character cell in most terminals
      const estimatedSixelWidthInChars = Math.floor(displayWidth / 12); // More accurate estimate
      const horizontalPadding = Math.max(1, Math.floor((termWidth - estimatedSixelWidthInChars) / 2));
      
      // Position cursor for optically centered display
      process.stdout.write(`\x1b[${verticalPadding};${horizontalPadding}H`);
      
      // Display the sixel image
      process.stdout.write(sixelData);
      
      // Hold the frame for half a second
      await new Promise(resolve => setTimeout(resolve, 500));
      
      // Return to main screen buffer
      process.stdout.write('\x1b[?1049l'); // Exit alt screen buffer
      
    } catch (error) {
      console.error('🚨 Sixel display error:', error.message);
    }
  }

  // Generate sixel data from RGB buffer (based on ultra-fast-sixel.mjs)
  generateSixel(rgbBuffer, width, height, maxWidth = 128, maxHeight = 128) {
    // Scale to fit within terminal dimensions while maintaining aspect ratio
    const aspectRatio = height / width;
    let targetWidth = Math.min(maxWidth, width);
    let targetHeight = Math.floor(targetWidth * aspectRatio);
    
    // If height is too large, scale based on height instead
    if (targetHeight > maxHeight) {
      targetHeight = maxHeight;
      targetWidth = Math.floor(targetHeight / aspectRatio);
    }
    
    const scaledWidth = targetWidth;
    const scaledHeight = targetHeight;
    
    // Calculate scaling factors
    const scaleX = width / scaledWidth;
    const scaleY = height / scaledHeight;
    
    // Create scaled buffer
    const scaledBuffer = new Uint8Array(scaledWidth * scaledHeight * 3);
    
    for (let y = 0; y < scaledHeight; y++) {
      for (let x = 0; x < scaledWidth; x++) {
        const srcX = Math.floor(x * scaleX);
        const srcY = Math.floor(y * scaleY);
        const srcIdx = (srcY * width + srcX) * 3;
        const dstIdx = (y * scaledWidth + x) * 3;
        
        scaledBuffer[dstIdx] = rgbBuffer[srcIdx];
        scaledBuffer[dstIdx + 1] = rgbBuffer[srcIdx + 1];
        scaledBuffer[dstIdx + 2] = rgbBuffer[srcIdx + 2];
      }
    }
    
    // Generate sixel without automatic positioning (let caller handle positioning)
    let output = '\x1bPq';
    
    const colors = new Map();
    let colorIndex = 0;
    
    // Process pixels in sixel bands (6 pixels high)
    for (let band = 0; band < Math.ceil(scaledHeight / 6); band++) {
      const bandData = new Map();
      
      // Collect colors for this band
      for (let x = 0; x < scaledWidth; x++) {
        for (let dy = 0; dy < 6; dy++) {
          const y = band * 6 + dy;
          if (y >= scaledHeight) break;
          
          const idx = (y * scaledWidth + x) * 3;
          const r = scaledBuffer[idx];
          const g = scaledBuffer[idx + 1];
          const b = scaledBuffer[idx + 2];
          
          const colorKey = `${r},${g},${b}`;
          
          if (!colors.has(colorKey)) {
            colors.set(colorKey, colorIndex);
            // Define color in sixel format
            output += `#${colorIndex};2;${Math.round(r * 100 / 255)};${Math.round(g * 100 / 255)};${Math.round(b * 100 / 255)}`;
            colorIndex++;
          }
          
          const color = colors.get(colorKey);
          
          if (!bandData.has(color)) {
            bandData.set(color, new Array(scaledWidth).fill(0));
          }
          
          // Set the bit for this pixel in the sixel band
          bandData.get(color)[x] |= (1 << dy);
        }
      }
      
      // Output band data
      for (const [color, pixels] of bandData) {
        output += `#${color}`;
        for (let x = 0; x < scaledWidth; x++) {
          output += String.fromCharCode(63 + pixels[x]);
        }
        output += '$';
      }
      output += '-'; // New line
    }
    
    return output + '\x1b\\'; // End sixel sequence
  }
}

function parseFrameCountArg(rawValue) {
  const defaultResult = () => ({
    frames: DEFAULT_FRAME_COUNT,
    seconds: DEFAULT_FRAME_COUNT / FRAMES_PER_SECOND,
  });

  if (rawValue === undefined || rawValue === null) {
    return defaultResult();
  }

  if (typeof rawValue === 'string' && rawValue.startsWith('--')) {
    return defaultResult();
  }

  const normalized = String(rawValue).trim().toLowerCase();
  if (!normalized) {
    return defaultResult();
  }

  let value = normalized;
  let unit = 'frames';

  if (value.endsWith('s')) {
    unit = 'seconds';
    value = value.slice(0, -1);
  } else if (value.endsWith('f')) {
    value = value.slice(0, -1);
  }

  const numeric = Number(value);
  if (!Number.isFinite(numeric)) {
    throw new Error(`Invalid duration value: ${rawValue}`);
  }

  const frames = unit === 'seconds'
    ? Math.max(1, Math.round(numeric * FRAMES_PER_SECOND))
    : Math.max(1, Math.round(numeric));

  return {
    frames,
    seconds: frames / FRAMES_PER_SECOND,
  };
}

// CLI usage
if (import.meta.url === `file://${process.argv[1]}`) {
  const rawArgs = process.argv.slice(2);
  const positional = [];
  const options = {
    gifMode: false,
    density: null,
    gifFps: null,
  };

  for (let i = 0; i < rawArgs.length; i++) {
    const arg = rawArgs[i];
    if (arg === '--gif') {
      options.gifMode = true;
      continue;
    }
    if (arg === '--density') {
      const value = rawArgs[i + 1];
      if (!value || value.startsWith('--')) {
        console.error('❌ --density flag requires a numeric value');
        process.exit(1);
      }
      options.density = parseFloat(value);
      if (Number.isNaN(options.density) || options.density <= 0) {
        console.error(`❌ Invalid density value: ${value}`);
        process.exit(1);
      }
      i += 1;
      continue;
    }
    if (arg === '--gif-fps') {
      const value = rawArgs[i + 1];
      if (!value || value.startsWith('--')) {
        console.error('❌ --gif-fps flag requires a numeric value');
        process.exit(1);
      }
      const parsed = parseInt(value, 10);
      if (Number.isNaN(parsed) || parsed <= 0) {
        console.error(`❌ Invalid GIF fps value: ${value}`);
        process.exit(1);
      }
      options.gifFps = parsed;
      i += 1;
      continue;
    }
    if (arg === '--gif-25' || arg === '--gif25') {
      options.gifFps = 25;
      continue;
    }
    if (arg === '--gif-50' || arg === '--gif50') {
      options.gifFps = 50;
      continue;
    }
    if (arg === '--gif-compress') {
      options.gifCompress = true;
      continue;
    }
    if (arg === '--sixel' || arg === 'sixel') {
      continue;
    }
    positional.push(arg);
  }

  let piece = positional[0] || 'elcid-flyer';
  let durationArg = positional[1];

  let frameInfo;
  try {
    frameInfo = parseFrameCountArg(durationArg);
  } catch (error) {
    console.error(`❌ ${error.message}`);
    process.exit(1);
  }

  const { frames, seconds: durationSecondsRaw } = frameInfo;
  // Allows values such as "3s" (seconds) or "180f" (frames); defaults to 300 frames (~5s)
  const width = parseInt(positional[2]) || 1024;
  const height = parseInt(positional[3]) || 1024;
  const durationSecondsLabel = Math.round(durationSecondsRaw * 10) / 10;
  
  // Handle kidlisp pieces (starting with $)
  if (piece.startsWith('$')) {
    console.log(`🎨 KidLisp piece detected: ${piece}`);
    // Keep piece as-is for kidlisp pieces
  } else {
    // Auto-resolve piece paths: if it's just a name, look in pieces/ folder
    if (piece && !piece.includes('/') && !piece.endsWith('.mjs')) {
      piece = path.join(__dirname, 'pieces', `${piece}.mjs`);
    } else if (piece && !piece.includes('/') && piece.endsWith('.mjs')) {
      piece = path.join(__dirname, 'pieces', piece);
    }
    // Otherwise use the piece path as-is (for absolute/relative paths)
  }
  
  // Create project-specific directory with piece name automatically  
  let outputDir;
  // Extract piece name from path, handling kidlisp pieces specially
  let pieceName;
  if (piece.startsWith('$')) {
    pieceName = piece.replace('$', ''); // Remove $ for directory name
  } else {
    pieceName = path.basename(piece, '.mjs');
  }
  // Create proper output directory with timestamp matching MP4 naming pattern
  const timestampStr = timestamp(); // Use AC timestamp format: YYYY.MM.DD.HH.MM.SS.mmm
  const durationSeconds = durationSecondsLabel; // Rounded to tenths for directory naming
  outputDir = path.resolve(__dirname, `../output/${pieceName}-${timestampStr}-${durationSecondsLabel}s`);
  
  // Ensure output directory exists
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
    console.log(`📁 Created output directory: ${outputDir}`);
  }
  
  let normalizedGifFps = options.gifFps;
  if (normalizedGifFps !== null && !ALLOWED_GIF_FPS.includes(normalizedGifFps)) {
    const nearest = ALLOWED_GIF_FPS.reduce((prev, curr) => {
      return Math.abs(curr - normalizedGifFps) < Math.abs(prev - normalizedGifFps) ? curr : prev;
    }, ALLOWED_GIF_FPS[0]);
    console.warn(`⚠️ GIF fps ${normalizedGifFps} unsupported. Using nearest supported value: ${nearest}`);
    normalizedGifFps = nearest;
  }

  const orchestrator = new RenderOrchestrator(piece, frames, outputDir, width, height, { gifMode: options.gifMode, density: options.density, gifFps: normalizedGifFps, gifCompress: options.gifCompress });
  orchestrator.renderAll().catch(console.error);
}

export { RenderOrchestrator };