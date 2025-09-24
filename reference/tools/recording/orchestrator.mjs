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

class RenderOrchestrator {
  constructor(piece, duration, outputDir, width = 2048, height = 2048) {
    this.piece = piece;
    this.duration = duration;
    this.outputDir = outputDir;
    this.width = width;
    this.height = height;
    this.frameRendererPath = path.join(__dirname, 'frame-renderer.mjs');
    this.isKidlispPiece = piece.startsWith('$');
  }

  // Fetch KidLisp source code from the localhost API (similar to tape.mjs)
  async fetchKidLispSource(code) {
    const cleanCode = code.replace(/^\$/, '');
    const url = `https://localhost:8888/.netlify/functions/store-kidlisp?code=${cleanCode}`;
    
    return new Promise((resolve, reject) => {
      const options = { rejectUnauthorized: false }; // Allow self-signed certificates
      
      https.get(url, options, (res) => {
        let data = '';
        res.on('data', (chunk) => data += chunk);
        res.on('end', () => {
          try {
            const response = JSON.parse(data);
            if (response.error) {
              reject(new Error(`KidLisp piece '${code}' not found`));
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
        reject(new Error(`Could not connect to localhost:8888 - make sure dev server is running`));
      });
    });
  }

  // Create a temporary kidlisp piece file for rendering
  async createKidlispPieceFile(source) {
    const pieceName = this.piece.replace('$', '');
    const pieceFilePath = path.resolve(this.outputDir, `${pieceName}-kidlisp.mjs`);
    
    // Create a piece file that uses the existing kidlisp() function
    const pieceContent = `
// Auto-generated kidlisp piece for recording: ${this.piece}
// Uses built-in kidlisp() function

export async function boot({ screen }) {
  console.log("🎨 Booting kidlisp piece: ${this.piece}");
}

export async function paint(api) {
  console.log("🔍 API object keys:", Object.keys(api));
  console.log("🔍 Has kidlisp?", typeof api.kidlisp);
  console.log("🔍 Has wipe?", typeof api.wipe);
  console.log("🔍 Has screen?", !!api.screen);
  
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
  }

  async renderAll() {
    console.log(`🎬 Starting stateless render: ${this.piece} for ${this.duration}ms`);
    console.log(`📁 Output: ${this.outputDir}`);
    
    let actualPiece = this.piece;
    
    // Handle kidlisp pieces
    if (this.isKidlispPiece) {
      console.log(`🎨 Detected kidlisp piece: ${this.piece}`);
      try {
        const source = await this.fetchKidLispSource(this.piece);
        console.log(`📄 Fetched kidlisp source (${source.length} chars)`);
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
        const result = execSync(
          `node ${this.frameRendererPath} ${actualPiece} ${this.duration} ${this.outputDir} ${this.width} ${this.height}`,
          { encoding: 'utf8', stdio: 'inherit' } // Changed to inherit to show frame timing output
        );
        
        frameCount++;
        consecutiveFailures = 0; // Reset on success
        
        // Check if complete by looking for state
        const stateFile = path.join(this.outputDir, 'state.json');
        if (fs.existsSync(stateFile)) {
          const state = JSON.parse(fs.readFileSync(stateFile, 'utf8'));
          complete = state.frameIndex >= state.totalFrames;
          
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
      // Now convert to MP4
      await this.convertToMP4();
      
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
      const durationSeconds = Math.round(this.duration / 60 * 10) / 10; // Convert frames to seconds at 60fps, round to 1 decimal place
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
      
      execSync(
        `ffmpeg -y -f rawvideo -pix_fmt rgb24 -s ${width}x${height} -r 60 ` +
        `-i ${allFramesFile} -c:v libx264 -pix_fmt yuv420p ` +
        `-movflags +faststart -r 60 -b:v 32M ${outputMP4}`,
        { stdio: 'inherit' }
      );
      
      console.log(`✅ MP4 created: ${filename}`);
      
    } catch (error) {
      console.error(`💥 MP4 conversion failed:`, error.message);
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
}

// CLI usage
if (import.meta.url === `file://${process.argv[1]}`) {
  let piece = process.argv[2] || 'elcid-flyer';
  const duration = parseInt(process.argv[3]) || 30; // Duration is frame count, not milliseconds
  const width = parseInt(process.argv[4]) || 1024;
  const height = parseInt(process.argv[5]) || 1024;
  
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
  const durationSeconds = Math.round(duration / 60 * 10) / 10; // Convert frames to seconds at 60fps, round to 1 decimal place
  outputDir = path.resolve(__dirname, `../output/${pieceName}-${timestampStr}-${durationSeconds}s`);
  
  // Ensure output directory exists
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
    console.log(`📁 Created output directory: ${outputDir}`);
  }
  
  const orchestrator = new RenderOrchestrator(piece, duration, outputDir, width, height);
  orchestrator.renderAll().catch(console.error);
}

export { RenderOrchestrator };