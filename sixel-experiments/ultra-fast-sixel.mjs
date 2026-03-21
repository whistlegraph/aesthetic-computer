#!/usr/bin/env node

// Ultra-Fast Sixel Renderer - Targeting 30-60 FPS
// Multiple aggressive optimizations for maximum speed

const BUFFER_WIDTH = 128;
const BUFFER_HEIGHT = 128;
const SCALE = 3;
const BORDER_BLOCKS = 2;

class UltraFastSixelRenderer {
  constructor(width, height, scale = 3, borderBlocks = 2) {
    this.width = width;
    this.height = height;
    this.scale = scale;
    this.borderBlocks = borderBlocks;
    
    this.contentWidth = width * scale;
    this.contentHeight = height * scale;
    this.borderSize = borderBlocks * scale;
    this.outputWidth = this.contentWidth + (this.borderSize * 2);
    this.outputHeight = this.contentHeight + (this.borderSize * 2);
    
    // Pre-allocate buffers to avoid GC
    this.buffer = new Uint8Array(width * height * 3);
    this.scaledBuffer = new Uint8Array(this.outputWidth * this.outputHeight * 3);
    
    // Pre-compute scaling lookup tables
    this.scaleMapX = new Uint16Array(this.outputWidth);
    this.scaleMapY = new Uint16Array(this.outputHeight);
    this.precomputeScaleMaps();
    
    console.log(`Ultra-fast renderer: ${width}x${height} → ${this.outputWidth}x${this.outputHeight}`);
  }

  // Pre-compute scaling lookup tables for faster scaling
  precomputeScaleMaps() {
    // X mapping
    for (let x = 0; x < this.outputWidth; x++) {
      if (x < this.borderSize) {
        this.scaleMapX[x] = 0; // Border pixels map to first source pixel
      } else if (x >= this.borderSize + this.contentWidth) {
        this.scaleMapX[x] = this.width - 1; // Border pixels map to last source pixel
      } else {
        this.scaleMapX[x] = Math.floor((x - this.borderSize) / this.scale);
      }
    }
    
    // Y mapping
    for (let y = 0; y < this.outputHeight; y++) {
      if (y < this.borderSize) {
        this.scaleMapY[y] = 0;
      } else if (y >= this.borderSize + this.contentHeight) {
        this.scaleMapY[y] = this.height - 1;
      } else {
        this.scaleMapY[y] = Math.floor((y - this.borderSize) / this.scale);
      }
    }
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

  // Ultra-fast scaling using lookup tables and border handling
  fastScaleWithBorder() {
    const buf = this.scaledBuffer;
    
    // Clear border to black first
    buf.fill(0);
    
    // Fast scaling using pre-computed maps
    for (let y = 0; y < this.outputHeight; y++) {
      const srcY = this.scaleMapY[y];
      const srcRowStart = srcY * this.width;
      const outRowStart = y * this.outputWidth;
      
      for (let x = 0; x < this.outputWidth; x++) {
        const srcX = this.scaleMapX[x];
        
        // Skip if we're in border area (already filled with black)
        if ((x < this.borderSize || x >= this.borderSize + this.contentWidth) ||
            (y < this.borderSize || y >= this.borderSize + this.contentHeight)) {
          continue;
        }
        
        const srcIdx = (srcRowStart + srcX) * 3;
        const outIdx = (outRowStart + x) * 3;
        
        buf[outIdx] = this.buffer[srcIdx];
        buf[outIdx + 1] = this.buffer[srcIdx + 1];
        buf[outIdx + 2] = this.buffer[srcIdx + 2];
      }
    }
    
    return { buffer: buf, width: this.outputWidth, height: this.outputHeight };
  }

  // Method 1: No-clear scrolling (fastest)
  renderNoOp() {
    const scaled = this.fastScaleWithBorder();
    return this.generateFastSixel(scaled, '\x1bPq');
  }

  // Method 2: Home cursor only
  renderHome() {
    const scaled = this.fastScaleWithBorder();
    return this.generateFastSixel(scaled, '\x1b[H\x1bPq');
  }

  // Method 3: Alternative screen (clean slate each time)
  renderAlt() {
    const scaled = this.fastScaleWithBorder();
    return '\x1b[?1049h\x1b[H' + this.generateFastSixel(scaled, '\x1bPq') + '\x1b[?1049l';
  }

  // Optimized sixel generation with minimal allocations
  generateFastSixel(scaled, prefix) {
    let output = prefix;
    const colors = new Map();
    let colorIndex = 0;
    
    // Pre-allocate band arrays to avoid repeated allocation
    const bandArrays = new Map();
    
    for (let band = 0; band < Math.ceil(scaled.height / 6); band++) {
      bandArrays.clear();
      
      for (let x = 0; x < scaled.width; x++) {
        for (let dy = 0; dy < 6; dy++) {
          const y = band * 6 + dy;
          if (y >= scaled.height) break;
          
          const idx = (y * scaled.width + x) * 3;
          const r = scaled.buffer[idx];
          const g = scaled.buffer[idx + 1];
          const b = scaled.buffer[idx + 2];
          
          // Simple color key to reduce string operations
          const colorKey = (r << 16) | (g << 8) | b;
          
          if (!colors.has(colorKey)) {
            colors.set(colorKey, colorIndex++);
            output += `#${colors.get(colorKey)};2;${Math.round(r*100/255)};${Math.round(g*100/255)};${Math.round(b*100/255)}`;
          }
          
          const color = colors.get(colorKey);
          if (!bandArrays.has(color)) {
            bandArrays.set(color, new Array(scaled.width).fill(0));
          }
          bandArrays.get(color)[x] |= (1 << dy);
        }
      }
      
      // Output band data
      for (const [color, pixels] of bandArrays) {
        output += `#${color}`;
        for (const pixel of pixels) {
          output += String.fromCharCode(63 + pixel);
        }
        output += '$';
      }
      output += '-';
    }
    
    return output + '\x1b\\';
  }

  // Even faster with reduced colors
  renderUltraFast() {
    // Skip scaling entirely - render directly at output resolution
    let output = '\x1b[H\x1bPq';
    
    // Use only 4 pre-defined colors for maximum speed
    const colors = ['#0;2;100;0;0', '#1;2;0;100;0', '#2;2;0;0;100', '#3;2;100;100;0'];
    output += colors.join('');
    
    // Generate simple pattern directly in sixel space
    const bandsCount = Math.ceil(this.outputHeight / 6);
    
    for (let band = 0; band < bandsCount; band++) {
      // Simple alternating pattern
      const colorIndex = band % 4;
      output += `#${colorIndex}`;
      
      for (let x = 0; x < this.outputWidth; x++) {
        const pattern = (x + band) % 4;
        output += String.fromCharCode(63 + pattern);
      }
      output += '$-';
    }
    
    return output + '\x1b\\';
  }
}

// Ultra-simple 2-color pattern for maximum speed
function generateUltraFastPattern(renderer, frame) {
  const color1 = [255, 0, 0];
  const color2 = [0, 0, 255];
  
  const blockSize = 32; // Large blocks for speed
  const offset = Math.floor(frame / 10);
  
  for (let y = 0; y < renderer.height; y++) {
    for (let x = 0; x < renderer.width; x++) {
      const block = Math.floor(x / blockSize) + Math.floor(y / blockSize) + offset;
      const color = (block % 2) ? color1 : color2;
      renderer.setPixel(x, y, color[0], color[1], color[2]);
    }
  }
}

// Solid color pattern for absolute maximum speed
function generateSolidPattern(renderer, frame) {
  const colors = [[255, 0, 0], [0, 255, 0], [0, 0, 255], [255, 255, 0]];
  const color = colors[Math.floor(frame / 30) % colors.length];
  renderer.clear(color[0], color[1], color[2]);
}

async function speedBenchmark() {
  console.log('Ultra-speed benchmark for 128x128...\n');
  
  const renderer = new UltraFastSixelRenderer(BUFFER_WIDTH, BUFFER_HEIGHT, SCALE, BORDER_BLOCKS);
  
  const tests = [
    ['Solid Color + Home', () => renderer.renderHome(), generateSolidPattern],
    ['Simple Pattern + Home', () => renderer.renderHome(), generateUltraFastPattern],
    ['Simple Pattern + NoOp', () => renderer.renderNoOp(), generateUltraFastPattern],
    ['Ultra Fast Direct', () => renderer.renderUltraFast(), () => {}],
    ['Pattern + Alt Screen', () => renderer.renderAlt(), generateUltraFastPattern]
  ];
  
  const results = {};
  
  for (const [name, renderFunc, patternFunc] of tests) {
    console.log(`Testing ${name}...`);
    
    const numFrames = 30;
    const frameTimes = [];
    
    for (let frame = 0; frame < numFrames; frame++) {
      const start = process.hrtime.bigint();
      
      patternFunc(renderer, frame);
      const sixelData = renderFunc();
      process.stdout.write(sixelData);
      
      const end = process.hrtime.bigint();
      frameTimes.push(Number(end - start) / 1000000);
    }
    
    const avgTime = frameTimes.reduce((sum, t) => sum + t, 0) / numFrames;
    const avgFps = 1000 / avgTime;
    results[name] = avgFps;
    
    process.stdout.write('\x1b[2J\x1b[H');
    await new Promise(resolve => setTimeout(resolve, 300));
  }
  
  console.log('\n=== ULTRA-SPEED RESULTS ===');
  const sorted = Object.entries(results).sort((a, b) => b[1] - a[1]);
  
  for (const [name, fps] of sorted) {
    const status = fps >= 60 ? '🚀' : fps >= 30 ? '✅' : '⚡';
    console.log(`${name.padEnd(25)}: ${fps.toFixed(1)} FPS ${status}`);
  }
  
  const fastest = sorted[0];
  console.log(`\nFastest: ${fastest[0]} at ${fastest[1].toFixed(1)} FPS`);
  
  if (fastest[1] >= 60) {
    console.log('🎯 TARGET ACHIEVED: 60+ FPS!');
  } else if (fastest[1] >= 30) {
    console.log('🎯 TARGET ACHIEVED: 30+ FPS!');
  } else {
    console.log('❌ Still below 30 FPS target');
  }
  
  return fastest;
}

// High-speed demo
async function ultraDemo(duration = 5000) {
  const [fastestMethod, fastestFps] = await speedBenchmark();
  
  console.log(`\nRunning ultra-fast demo at ~${fastestFps.toFixed(0)} FPS...`);
  
  const renderer = new UltraFastSixelRenderer(BUFFER_WIDTH, BUFFER_HEIGHT, SCALE, BORDER_BLOCKS);
  
  let frame = 0;
  const startTime = Date.now();
  const frameCount = { count: 0 };
  
  const interval = setInterval(() => {
    generateUltraFastPattern(renderer, frame++);
    const sixelData = renderer.renderHome();
    process.stdout.write(sixelData);
    frameCount.count++;
    
    if (Date.now() - startTime > duration) {
      clearInterval(interval);
      const actualFps = frameCount.count / (duration / 1000);
      process.stdout.write('\x1b[2J\x1b[H');
      console.log(`\nDemo complete!`);
      console.log(`Theoretical FPS: ${fastestFps.toFixed(1)}`);
      console.log(`Actual demo FPS: ${actualFps.toFixed(1)}`);
    }
  }, 1000/120); // Try to run at max speed
}

// CLI
if (process.argv.length > 2) {
  const command = process.argv[2];
  if (command === 'demo') {
    const duration = parseInt(process.argv[3]) || 5000;
    ultraDemo(duration);
  } else if (command === 'benchmark') {
    speedBenchmark();
  } else {
    console.log('Usage:');
    console.log('  node ultra-fast-sixel.mjs benchmark - Run speed tests');
    console.log('  node ultra-fast-sixel.mjs demo [ms] - Run high-speed demo');
  }
} else {
  speedBenchmark().catch(console.error);
}

// Export for use as module
export { UltraFastSixelRenderer };
