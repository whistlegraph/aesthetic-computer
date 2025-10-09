#!/usr/bin/env node

/**
 * GIF Renderer for Cover Images
 * Generates a short animated GIF suitable for use as cover images in OBJKT packages
 * Based on the orchestrator system but optimized for small file sizes
 * Uses gifenc for high-quality GIF generation
 */

import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { GIFEncoder, quantize, applyPalette, prequantize } from '../../../system/public/aesthetic.computer/dep/gifenc/gifenc.esm.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

class GifRenderer {
  constructor(piece, outputPath, options = {}) {
    this.piece = piece;
    this.outputPath = outputPath;
    this.frameRendererPath = path.join(__dirname, 'frame-renderer.mjs');
    
    // GIF-optimized settings
    this.options = {
      duration: options.duration || 3000, // 3 seconds default
      fps: options.fps || 12, // Lower fps for smaller files
      width: options.width || 400, // Smaller resolution
      height: options.height || 400,
      ...options
    };
    
    // Create temp directory for frames
    this.tempDir = path.join(path.dirname(outputPath), '.gif-temp-' + Date.now());
    
    // Check if this is a KidLisp code (starts with $)
    this.isKidLisp = piece.startsWith('$');
  }

  async generateGif() {
    console.log(`🎞️ Generating GIF cover: ${path.basename(this.outputPath)}`);
    console.log(`📐 Size: ${this.options.width}×${this.options.height}, ${this.options.fps}fps, ${this.options.duration}ms`);
    
    try {
      // Create temp directory
      fs.mkdirSync(this.tempDir, { recursive: true });
      
      const startTime = Date.now();
      let frameCount = 0;
      let complete = false;
      let consecutiveFailures = 0;
      const maxRetries = 3;
      
      while (!complete && consecutiveFailures < maxRetries) {
        try {
          // Run one frame in fresh process with our custom resolution
          const result = execSync(
            `WIDTH=${this.options.width} HEIGHT=${this.options.height} GIF_MODE=true node ${this.frameRendererPath} ${this.piece} ${this.options.duration} ${this.tempDir}`,
            { encoding: 'utf8', stdio: 'inherit' } // Show output for debugging
          );
          
          frameCount++;
          consecutiveFailures = 0;
          
          // Check if complete
          const stateFile = path.join(this.tempDir, 'state.json');
          if (fs.existsSync(stateFile)) {
            const state = JSON.parse(fs.readFileSync(stateFile, 'utf8'));
            complete = state.frameIndex >= state.totalFrames;
            
            // Show simple progress
            if (frameCount % 10 === 0) {
              console.log(`🎞️ Rendered ${frameCount}/${state.totalFrames} frames...`);
            }
          }
          
        } catch (error) {
          consecutiveFailures++;
          console.warn(`⚠️ Frame ${frameCount} failed (${consecutiveFailures}/${maxRetries})`);
          
          if (consecutiveFailures >= maxRetries) {
            throw new Error(`Too many consecutive failures rendering frames`);
          }
        }
      }
      
      if (complete) {
        console.log(`✅ Rendered ${frameCount} frames, converting to GIF...`);
        await this.convertToGif();
        console.log(`🎞️ GIF created: ${path.basename(this.outputPath)}`);
        return true;
      }
      
      return false;
      
    } catch (error) {
      console.error(`💥 GIF generation failed:`, error.message);
      return false;
    } finally {
      // Cleanup temp directory
      this.cleanup();
    }
  }

  async convertToGif() {
    try {
      console.log(`🎞️ Converting frames to GIF using gifenc...`);
      
      // Get RGB frame files
      const frameFiles = fs.readdirSync(this.tempDir).filter(f => f.startsWith('frame-') && f.endsWith('.rgb'));
      frameFiles.sort((a, b) => {
        const aNum = parseInt(a.match(/frame-(\d+)/)[1]);
        const bNum = parseInt(b.match(/frame-(\d+)/)[1]);
        return aNum - bNum;
      });

      if (frameFiles.length === 0) {
        throw new Error('No frame files found for GIF conversion');
      }

      // Sample frames for palette generation (use every frame for short GIFs)
      console.log(`🎨 Generating palette from ${frameFiles.length} frames...`);
      const samplePixels = [];
      
      for (let i = 0; i < frameFiles.length; i++) {
        const rgbFile = path.join(this.tempDir, frameFiles[i]);
        const rgbData = fs.readFileSync(rgbFile);
        
        // Convert RGB to RGBA (add alpha channel)
        const rgbaData = new Uint8ClampedArray(rgbData.length / 3 * 4);
        for (let j = 0, k = 0; j < rgbData.length; j += 3, k += 4) {
          rgbaData[k] = rgbData[j];       // R
          rgbaData[k + 1] = rgbData[j + 1]; // G
          rgbaData[k + 2] = rgbData[j + 2]; // B
          rgbaData[k + 3] = 255;          // A (fully opaque)
        }
        
        samplePixels.push(rgbaData);
      }
      
      // Combine all sample pixels for palette generation
      const totalPixels = samplePixels.reduce((sum, pixels) => sum + pixels.length, 0);
      const allSamplePixels = new Uint8ClampedArray(totalPixels);
      let offset = 0;
      
      for (const pixels of samplePixels) {
        allSamplePixels.set(pixels, offset);
        offset += pixels.length;
      }
      
      // Apply prequantization for better GIF compression
      console.log(`� Prequantizing ${(totalPixels / 4)} pixels...`);
      prequantize(allSamplePixels, {
        roundRGB: 5,
        roundAlpha: 20,
        oneBitAlpha: null
      });
      
      // Generate optimized palette
      const palette = quantize(allSamplePixels, 128, { // Use 128 colors for smaller files
        format: "rgba4444",
        clearAlpha: false,
        oneBitAlpha: false,
        colorSpace: "rgb"
      });
      
      console.log(`🎨 Generated palette with ${palette.length} colors`);
      
      // Create GIF encoder
      const frameDelayMs = Math.round(1000 / this.options.fps);
      const gif = GIFEncoder({
        auto: true,
        initialCapacity: 4096
      });
      
      // Process each frame
      console.log(`🖼️ Processing ${frameFiles.length} frames...`);
      for (let i = 0; i < frameFiles.length; i++) {
        const rgbFile = path.join(this.tempDir, frameFiles[i]);
        const rgbData = fs.readFileSync(rgbFile);
        
        // Convert RGB to RGBA
        const rgbaData = new Uint8ClampedArray(rgbData.length / 3 * 4);
        for (let j = 0, k = 0; j < rgbData.length; j += 3, k += 4) {
          rgbaData[k] = rgbData[j];
          rgbaData[k + 1] = rgbData[j + 1];
          rgbaData[k + 2] = rgbData[j + 2];
          rgbaData[k + 3] = 255;
        }
        
        // Apply palette to get indexed data
        const indexedData = applyPalette(rgbaData, palette, "rgba4444");
        
        // Write frame to GIF
        if (i === 0) {
          gif.writeFrame(indexedData, this.options.width, this.options.height, {
            palette: palette,
            delay: frameDelayMs,
            repeat: 0, // Loop forever
            transparent: false,
            dispose: -1
          });
        } else {
          gif.writeFrame(indexedData, this.options.width, this.options.height, {
            delay: frameDelayMs,
            transparent: false,
            dispose: -1
          });
        }
      }
      
      // Generate final GIF buffer and save
      const gifBuffer = gif.bytes();
      fs.writeFileSync(this.outputPath, gifBuffer);
      
      console.log(`✅ GIF created: ${(gifBuffer.length / 1024).toFixed(1)} KB`);
      
    } catch (error) {
      throw new Error(`GIF conversion failed: ${error.message}`);
    }
  }

  cleanup() {
    try {
      if (fs.existsSync(this.tempDir)) {
        fs.rmSync(this.tempDir, { recursive: true, force: true });
      }
    } catch (error) {
      console.warn(`⚠️ Cleanup warning: ${error.message}`);
    }
  }
}

// CLI usage
if (import.meta.url === `file://${process.argv[1]}`) {
  const piece = process.argv[2] || 'test';
  const outputPath = process.argv[3] || 'cover.gif';
  const duration = parseInt(process.argv[4]) || 3000;
  
  // Resolve piece path
  let piecePath = piece;
  if (!piece.includes('/') && !piece.endsWith('.mjs')) {
    piecePath = path.join(__dirname, 'pieces', `${piece}.mjs`);
  }
  
  const renderer = new GifRenderer(piecePath, outputPath, { duration });
  renderer.generateGif().catch(console.error);
}

export { GifRenderer };