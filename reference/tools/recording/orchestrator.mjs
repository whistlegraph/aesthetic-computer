#!/usr/bin/env node

/**
 * Orchestrator Script
 * Manages the stateless frame-by-frame rendering
 */

import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

class RenderOrchestrator {
  constructor(piece, duration, outputDir) {
    this.piece = piece;
    this.duration = duration;
    this.outputDir = outputDir;
    this.frameRendererPath = path.join(__dirname, 'frame-renderer.mjs');
  }

  async renderAll() {
    console.log(`🎬 Starting stateless render: ${this.piece} for ${this.duration}ms`);
    console.log(`📁 Output: ${this.outputDir}`);
    
    const startTime = Date.now();
    let frameCount = 0;
    let complete = false;
    let consecutiveFailures = 0;
    const maxRetries = 3;
    
    while (!complete && consecutiveFailures < maxRetries) {
      try {
        // Run one frame in fresh process
        const result = execSync(
          `node ${this.frameRendererPath} ${this.piece} ${this.duration} _ ${this.outputDir}`,
          { encoding: 'utf8', stdio: 'inherit' } // Changed to inherit to show frame timing output
        );
        
        frameCount++;
        consecutiveFailures = 0; // Reset on success
        
        // Check if complete by looking for state
        const stateFile = path.join(this.outputDir, 'state.json');
        if (fs.existsSync(stateFile)) {
          const state = JSON.parse(fs.readFileSync(stateFile, 'utf8'));
          complete = state.frameIndex >= state.totalFrames;
          
          if (frameCount % 5 === 0) {
            const avgFrameTime = frameCount > 0 ? Math.round((Date.now() - startTime) / frameCount) : 0;
            console.log(`📊 Progress: ${state.frameIndex}/${state.totalFrames} frames (${Math.round(state.frameIndex/state.totalFrames*100)}%) | Avg: ${avgFrameTime}ms/frame`);
          }
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
    }
  }

  async convertToMP4() {
    console.log(`🎬 Converting frames to MP4...`);
    
    try {
      // First concatenate all frame files into one RGB file
      console.log(`📦 Concatenating frame files...`);
      const allFramesFile = path.join(this.outputDir, 'all-frames.rgb');
      execSync(`cat ${this.outputDir}/frame-*.rgb > ${allFramesFile}`, { stdio: 'inherit' });
      
      // Convert the concatenated RGB file to MP4
      const outputMP4 = path.join(this.outputDir, 'video.mp4');
      
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
        `-movflags +faststart -r 60 -b:v 16M ${outputMP4}`,
        { stdio: 'inherit' }
      );
      
      console.log(`✅ MP4 created: ${outputMP4}`);
      
    } catch (error) {
      console.error(`💥 MP4 conversion failed:`, error.message);
    }
  }
}

// CLI usage
if (import.meta.url === `file://${process.argv[1]}`) {
  const piece = process.argv[2] || 'reference/tools/recording/elcid-flyer.mjs';
  const duration = parseInt(process.argv[3]) || 1000;
  
  // Create project-specific directory with piece name and timestamp in /output directory
  let outputDir = process.argv[4];
  if (!outputDir) {
    // Extract piece name from path (e.g., "elcid-flyer" from "reference/tools/recording/elcid-flyer.mjs")
    const pieceName = path.basename(piece, '.mjs');
    const timestamp = new Date().toISOString().slice(0, 19).replace(/:/g, '-'); // YYYY-MM-DDTHH-MM-SS
    outputDir = `../output/${pieceName}-${timestamp}`;
  }
  
  // Ensure output directory exists
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
    console.log(`📁 Created output directory: ${outputDir}`);
  }
  
  const orchestrator = new RenderOrchestrator(piece, duration, outputDir);
  orchestrator.renderAll().catch(console.error);
}

export { RenderOrchestrator };