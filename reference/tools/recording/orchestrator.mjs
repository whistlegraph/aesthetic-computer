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
    
    // Clear screen and hide cursor for progress bar
    process.stdout.write('\x1b[2J\x1b[H\x1b[?25l');
    
    while (!complete && consecutiveFailures < maxRetries) {
      try {
        // Run one frame in fresh process
        const result = execSync(
          `node ${this.frameRendererPath} ${this.piece} ${this.duration} ${this.outputDir}`,
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
      const pieceName = path.basename(this.piece, '.mjs');
      const timestamp = this.generateTimestamp();
      const durationSeconds = Math.round(this.duration / 1000 * 10) / 10; // Round to 1 decimal place
      const filename = `${pieceName}-${timestamp}-${durationSeconds}s.mp4`;
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

  // Generate timestamp in AC format: YYYY.MM.DD.HH.MM.SS.mmm (no zero-padding except milliseconds)
  generateTimestamp() {
    const d = new Date();
    const year = d.getFullYear();
    const month = d.getMonth() + 1; // getMonth() returns 0-11
    const day = d.getDate();
    const hour = d.getHours();
    const minute = d.getMinutes();
    const second = d.getSeconds();
    const millisecond = d.getMilliseconds();
    
    // Match AC timestamp format exactly: no zero-padding except for milliseconds
    return `${year}.${month}.${day}.${hour}.${minute}.${second}.${millisecond.toString().padStart(3, "0")}`;
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
  const duration = parseInt(process.argv[3]) || 1000;
  
  // Auto-resolve piece paths: if it's just a name, look in pieces/ folder
  if (piece && !piece.includes('/') && !piece.endsWith('.mjs')) {
    piece = path.join(__dirname, 'pieces', `${piece}.mjs`);
  } else if (piece && !piece.includes('/') && piece.endsWith('.mjs')) {
    piece = path.join(__dirname, 'pieces', piece);
  }
  // Otherwise use the piece path as-is (for absolute/relative paths)
  
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