// Test with real embedded content to generate a proper GIF

import { HeadlessAC } from './headless.mjs';
import { KidLisp } from '../../../system/public/aesthetic.computer/lib/kidlisp.mjs';
import { GIFEncoder, quantize, applyPalette } from '../../../system/public/aesthetic.computer/dep/gifenc/gifenc.esm.js';
import { writeFileSync } from 'fs';
import { timestamp } from "../../../system/public/aesthetic.computer/lib/num.mjs";

class FixedTape extends HeadlessAC {
  constructor(width = 256, height = 256) {
    super(width, height);
    this.frames = [];
  }

  async recordWithRealContent() {
    console.log('🎬 Recording $cow with real embedded content...');
    
    // Create the API
    const api = this.createAPI();
    
    // Create KidLisp instance
    const kidlisp = new KidLisp();
    
    // Pre-populate with realistic embedded content
    kidlisp.embeddedSourceCache = new Map();
    kidlisp.embeddedSourceCache.set('39i', `
(fps 24)
(wipe 255 100 100)
(ink 255 255 255)
(write "39i" 10 10)
(box 20 20 60 60)
(circle 50 50 20)
`);
    
    kidlisp.embeddedSourceCache.set('r2f', `
(fps 24)
(wipe 100 255 100)
(ink 0 0 0)
(write "r2f" 10 10)
(circle 30 30 25)
(box 10 50 40 20)
`);
    
    // Set API on KidLisp instance
    kidlisp.api = api;
    
    // Parse and create the cow module with embedded content
    const cowSource = `
(wipe 50 50 50)
(ink 255 255 0)
(write "COW" 10 10)
($39i 50 50 100 100 180)
($r2f 150 50 100 100 180)
(contrast 1.2)
`;
    
    console.log('📝 Cow source with embedded pieces:');
    console.log(cowSource);
    
    const parsed = kidlisp.parse(cowSource);
    
    // Record 5 frames
    for (let frame = 0; frame < 5; frame++) {
      console.log(`🎬 Recording frame ${frame + 1}/5`);
      
      // Clear the screen
      this.graph.clear();
      
      // Evaluate the KidLisp code
      kidlisp.evaluate(parsed, api);
      
      // Capture frame
      const frameData = new Uint8ClampedArray(this.pixelBuffer);
      this.frames.push(frameData);
    }
    
    // Export GIF
    await this.exportGIF();
  }
  
  async exportGIF() {
    console.log('🎞️ Exporting GIF with real content...');
    
    const gif = GIFEncoder();
    
    for (let i = 0; i < this.frames.length; i++) {
      const frame = this.frames[i];
      
      // Convert RGBA to RGB
      const rgb = new Uint8Array(this.width * this.height * 3);
      for (let j = 0; j < this.width * this.height; j++) {
        rgb[j * 3] = frame[j * 4];     // R
        rgb[j * 3 + 1] = frame[j * 4 + 1]; // G  
        rgb[j * 3 + 2] = frame[j * 4 + 2]; // B
      }
      
      const palette = quantize(rgb, 256);
      const index = applyPalette(rgb, palette);
      
      gif.writeFrame(index, this.width, this.height, {
        palette,
        delay: 200 // 200ms = 5fps
      });
    }
    
    gif.finish();
    
    const buffer = gif.bytes();
    const filename = `fixed-cow-${timestamp()}.gif`;
    writeFileSync(filename, buffer);
    
    console.log(`✅ Fixed GIF saved: ${filename}`);
    console.log(`📊 File size: ${(buffer.length / 1024).toFixed(1)} KB`);
    
    return filename;
  }
}

// Run the test
const tape = new FixedTape();
tape.recordWithRealContent().catch(console.error);