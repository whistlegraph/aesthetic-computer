#!/usr/bin/env node

/**
 * Television.mjs - Sixel Preview Generator
 * Watches for raw RGBA frame files and converts them to sixel for terminal display
 */

import { readFileSync, existsSync, watchFile, statSync } from 'fs';
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

// Simple Sixel Renderer for terminal display
class SixelRenderer {
  constructor(width, height) {
    this.width = width;
    this.height = height;
    this.buffer = new Uint8Array(width * height * 3); // RGB buffer
  }

  setPixel(x, y, r, g, b) {
    if (x >= 0 && x < this.width && y >= 0 && y < this.height) {
      const idx = (y * this.width + x) * 3;
      this.buffer[idx] = r;
      this.buffer[idx + 1] = g;
      this.buffer[idx + 2] = b;
    }
  }

  clear(r = 0, g = 0, b = 0) {
    for (let i = 0; i < this.buffer.length; i += 3) {
      this.buffer[i] = r;
      this.buffer[i + 1] = g;
      this.buffer[i + 2] = b;
    }
  }

  // Render sixel output for terminal
  render() {
    let output = '\x1bPq';
    
    const colors = new Map();
    let colorIndex = 0;
    
    // Process pixels in sixel bands (6 pixels high)
    for (let band = 0; band < Math.ceil(this.height / 6); band++) {
      const bandData = new Map();
      
      for (let x = 0; x < this.width; x++) {
        for (let dy = 0; dy < 6; dy++) {
          const y = band * 6 + dy;
          if (y >= this.height) break;
          
          const idx = (y * this.width + x) * 3;
          const r = this.buffer[idx];
          const g = this.buffer[idx + 1];
          const b = this.buffer[idx + 2];
          const colorKey = `${r},${g},${b}`;
          
          if (!colors.has(colorKey)) {
            colors.set(colorKey, colorIndex++);
            output += `#${colors.get(colorKey)};2;${Math.round(r*100/255)};${Math.round(g*100/255)};${Math.round(b*100/255)}`;
          }
          
          const color = colors.get(colorKey);
          if (!bandData.has(color)) {
            bandData.set(color, new Array(this.width).fill(0));
          }
          bandData.get(color)[x] |= (1 << dy);
        }
      }
      
      // Output this band
      for (const [color, pixels] of bandData) {
        output += `#${color}`;
        for (const pixel of pixels) {
          output += String.fromCharCode(63 + pixel);
        }
        output += '$'; // New line
      }
      output += '-'; // Next band
    }
    
    // End sixel
    output += '\x1b\\';
    return output;
  }
}

class Television {
  constructor(width = 2048, height = 2048, frameDir = '.tape-frames-cache') {
    this.width = width;
    this.height = height;
    this.frameDir = frameDir;
    this.lastFrameIndex = -1;
    this.isGenerating = false;
    // Create a smaller sixel renderer for preview (128x128 or 64x64)
    this.sixelRenderer = new SixelRenderer(64, 64); // Compact preview size
  }

  async init() {
    console.log(`📺 Television sixel preview starting - watching ${this.frameDir}`);
    this.watchForFrames();
  }

  watchForFrames() {
    // Check for new frames every 500ms
    setInterval(async () => {
      if (this.isGenerating) return;
      
      // Look for the latest frame file
      let latestFrame = -1;
      for (let i = 0; i < 100; i++) { // Check up to 100 frames
        const framePath = `${this.frameDir}/frame-${i.toString().padStart(6, '0')}.rgba`;
        if (existsSync(framePath)) {
          latestFrame = i;
        }
      }
      
      if (latestFrame > this.lastFrameIndex) {
        this.lastFrameIndex = latestFrame;
        await this.generateSixel(latestFrame);
      }
    }, 500);
  }

  async generateSixel(frameIndex) {
    if (this.isGenerating) return;
    this.isGenerating = true;

    try {
      const framePath = `${this.frameDir}/frame-${frameIndex.toString().padStart(6, '0')}.rgba`;
      
      if (!existsSync(framePath)) {
        console.log(`📺 Frame ${frameIndex} not found: ${framePath}`);
        return;
      }

      // Read raw RGBA data
      const rgbaData = readFileSync(framePath);
      const expectedSize = this.width * this.height * 4; // RGBA = 4 bytes per pixel
      
      if (rgbaData.length !== expectedSize) {
        console.log(`📺 Frame ${frameIndex}: Invalid size ${rgbaData.length} (expected ${expectedSize})`);
        return;
      }

      // Clear the sixel renderer
      this.sixelRenderer.clear(0, 0, 0);
      
      // Scale down and convert RGBA to RGB for sixel renderer
      const scaleX = this.width / this.sixelRenderer.width;
      const scaleY = this.height / this.sixelRenderer.height;
      
      for (let sy = 0; sy < this.sixelRenderer.height; sy++) {
        for (let sx = 0; sx < this.sixelRenderer.width; sx++) {
          // Sample from source image
          const sourceX = Math.floor(sx * scaleX);
          const sourceY = Math.floor(sy * scaleY);
          const sourceIdx = (sourceY * this.width + sourceX) * 4; // RGBA
          
          if (sourceIdx + 2 < rgbaData.length) {
            const r = rgbaData[sourceIdx];
            const g = rgbaData[sourceIdx + 1];
            const b = rgbaData[sourceIdx + 2];
            // Alpha is at sourceIdx + 3 but we ignore it
            
            this.sixelRenderer.setPixel(sx, sy, r, g, b);
          }
        }
      }
      
      // Generate both ASCII preview (for blessed.js) and sixel (for overlay)
      const asciiPreview = this.generateAsciiPreview(rgbaData);
      const sixelOutput = this.sixelRenderer.render();
      
      // Output ASCII preview for dashboard consumption (blessed.js compatible)
      console.log(`sixel preview:\n${asciiPreview}`);
      
      // Also output real sixel for experimental overlay
      console.log(`sixel overlay:\n${sixelOutput}`);
      
    } catch (error) {
      console.error(`📺 Error generating sixel for frame ${frameIndex}:`, error.message);
    } finally {
      this.isGenerating = false;
    }
  }

  // Generate ASCII representation of the frame for blessed.js compatibility
  generateAsciiPreview(rgbaData) {
    const previewWidth = 32; // Compact ASCII preview
    const previewHeight = 16;
    const scaleX = this.width / previewWidth;
    const scaleY = this.height / previewHeight;
    
    // ASCII characters for different brightness levels
    const asciiChars = ' .:-=+*#%@';
    
    let asciiOutput = '';
    
    for (let y = 0; y < previewHeight; y++) {
      for (let x = 0; x < previewWidth; x++) {
        // Sample from source image
        const sourceX = Math.floor(x * scaleX);
        const sourceY = Math.floor(y * scaleY);
        const sourceIdx = (sourceY * this.width + sourceX) * 4; // RGBA
        
        if (sourceIdx + 2 < rgbaData.length) {
          const r = rgbaData[sourceIdx];
          const g = rgbaData[sourceIdx + 1];
          const b = rgbaData[sourceIdx + 2];
          
          // Convert to grayscale for ASCII mapping
          const gray = Math.floor(0.299 * r + 0.587 * g + 0.114 * b);
          const charIndex = Math.floor((gray / 255) * (asciiChars.length - 1));
          asciiOutput += asciiChars[charIndex];
        } else {
          asciiOutput += ' ';
        }
      }
      asciiOutput += '\n';
    }
    
    return `📺 Frame Preview (${previewWidth}x${previewHeight})\n${asciiOutput}`;
  }
}

// Main execution
async function main() {
  const args = process.argv.slice(2);
  const width = parseInt(args[0]) || 2048;
  const height = parseInt(args[1]) || 2048;
  const frameDir = args[2] || '.tape-frames-cache';

  const tv = new Television(width, height, frameDir);
  await tv.init();
  
  // Keep process alive
  process.on('SIGINT', () => {
    console.log('\n📺 Television shutting down...');
    process.exit(0);
  });
}

main().catch(console.error);