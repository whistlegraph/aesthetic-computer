#!/usr/bin/env node

// Optimized 64x64 Sixel Renderer with Black Border
// High performance version with bordered display

const BUFFER_WIDTH = 128;
const BUFFER_HEIGHT = 128;
const SCALE = 3;
const BORDER_BLOCKS = 2; // Black border thickness in buffer blocks (will be scaled)

class BorderedSixelRenderer {
  constructor(width, height, scale = 3, borderBlocks = 2) {
    this.width = width;
    this.height = height;
    this.scale = scale;
    this.borderBlocks = borderBlocks;
    
    // Calculate output dimensions including border (border is also scaled)
    this.contentWidth = width * scale;
    this.contentHeight = height * scale;
    this.borderSize = borderBlocks * scale; // Border is scaled like pixels
    this.outputWidth = this.contentWidth + (this.borderSize * 2);
    this.outputHeight = this.contentHeight + (this.borderSize * 2);
    
    this.buffer = new Uint8Array(width * height * 3); // RGB buffer
    
    console.log(`Initialized ${width}x${height} → ${this.outputWidth}x${this.outputHeight} (${scale}x scale + ${borderBlocks}×${scale}=${this.borderSize}px chunky border)`);
  }

  setPixel(x, y, r, g, b) {
    if (x >= 0 && x < this.width && y >= 0 && y < this.height) {
      const idx = (y * this.width + x) * 3;
      this.buffer[idx] = r;
      this.buffer[idx + 1] = g;
      this.buffer[idx + 2] = b;
    }
  }

  fillRect(x, y, w, h, r, g, b) {
    for (let dy = 0; dy < h; dy++) {
      for (let dx = 0; dx < w; dx++) {
        this.setPixel(x + dx, y + dy, r, g, b);
      }
    }
  }

  clear(r = 0, g = 0, b = 0) {
    for (let i = 0; i < this.buffer.length; i += 3) {
      this.buffer[i] = r;
      this.buffer[i + 1] = g;
      this.buffer[i + 2] = b;
    }
  }

  // Scale buffer and add chunky black border
  scaleBufferWithBorder() {
    const outputBuffer = new Uint8Array(this.outputWidth * this.outputHeight * 3);
    
    // Fill entire output with black (border color)
    outputBuffer.fill(0);
    
    // Scale and place content in the center, offset by scaled border
    for (let y = 0; y < this.contentHeight; y++) {
      for (let x = 0; x < this.contentWidth; x++) {
        const srcX = Math.floor(x / this.scale);
        const srcY = Math.floor(y / this.scale);
        const srcIdx = (srcY * this.width + srcX) * 3;
        
        // Output position including scaled border offset
        const outX = x + this.borderSize;
        const outY = y + this.borderSize;
        const outIdx = (outY * this.outputWidth + outX) * 3;
        
        outputBuffer[outIdx] = this.buffer[srcIdx];
        outputBuffer[outIdx + 1] = this.buffer[srcIdx + 1];
        outputBuffer[outIdx + 2] = this.buffer[srcIdx + 2];
      }
    }
    
    return { 
      buffer: outputBuffer, 
      width: this.outputWidth, 
      height: this.outputHeight 
    };
  }

  // Optimized cursor save/restore rendering with border
  render() {
    const scaled = this.scaleBufferWithBorder();
    
    // Save cursor position + start sixel
    let output = '\x1b[s\x1bPq';
    
    const colors = new Map();
    let colorIndex = 0;
    
    // Process pixels in sixel bands (6 pixels high)
    for (let band = 0; band < Math.ceil(scaled.height / 6); band++) {
      const bandData = new Map();
      
      for (let x = 0; x < scaled.width; x++) {
        for (let dy = 0; dy < 6; dy++) {
          const y = band * 6 + dy;
          if (y >= scaled.height) break;
          
          const idx = (y * scaled.width + x) * 3;
          const r = scaled.buffer[idx];
          const g = scaled.buffer[idx + 1];
          const b = scaled.buffer[idx + 2];
          const colorKey = `${r},${g},${b}`;
          
          if (!colors.has(colorKey)) {
            colors.set(colorKey, colorIndex++);
            output += `#${colors.get(colorKey)};2;${Math.round(r*100/255)};${Math.round(g*100/255)};${Math.round(b*100/255)}`;
          }
          
          const color = colors.get(colorKey);
          if (!bandData.has(color)) {
            bandData.set(color, new Array(scaled.width).fill(0));
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
    
    // End sixel + restore cursor
    output += '\x1b\\\x1b[u';
    return output;
  }
}

// Demo animation patterns optimized for 128x128
const DemoPatterns = {
  // Animated color blocks
  colorBlocks: (renderer, frame) => {
    const colors = [
      [255, 0, 0], [0, 255, 0], [0, 0, 255], [255, 255, 0],
      [255, 0, 255], [0, 255, 255], [255, 128, 0], [128, 0, 255]
    ];
    
    const blockSize = 16; // Larger blocks for 128x128
    const timeOffset = Math.floor(frame / 5);
    
    for (let y = 0; y < renderer.height; y++) {
      for (let x = 0; x < renderer.width; x++) {
        const blockX = Math.floor(x / blockSize);
        const blockY = Math.floor(y / blockSize);
        const colorIndex = (blockX + blockY + timeOffset) % colors.length;
        const color = colors[colorIndex];
        renderer.setPixel(x, y, color[0], color[1], color[2]);
      }
    }
  },

  // Plasma effect
  plasma: (renderer, frame) => {
    const colors = [
      [255, 0, 0], [255, 128, 0], [255, 255, 0], [0, 255, 0],
      [0, 255, 255], [0, 0, 255], [128, 0, 255], [255, 0, 255]
    ];
    
    for (let y = 0; y < renderer.height; y++) {
      for (let x = 0; x < renderer.width; x++) {
        const plasma = Math.sin(x * 0.1 + frame * 0.1) + 
                      Math.cos(y * 0.1 + frame * 0.05) +
                      Math.sin((x + y) * 0.05 + frame * 0.02);
        const colorIndex = Math.floor((plasma + 3) * colors.length / 6) % colors.length;
        const color = colors[colorIndex];
        renderer.setPixel(x, y, color[0], color[1], color[2]);
      }
    }
  },

  // Moving stripes
  stripes: (renderer, frame) => {
    const colors = [[255, 0, 0], [0, 255, 0], [0, 0, 255], [255, 255, 0]];
    
    for (let y = 0; y < renderer.height; y++) {
      for (let x = 0; x < renderer.width; x++) {
        const stripe = Math.floor((x + frame) / 8) % colors.length; // Medium stripes
        const color = colors[stripe];
        renderer.setPixel(x, y, color[0], color[1], color[2]);
      }
    }
  },

  // Bouncing ball
  bouncingBall: (renderer, frame) => {
    renderer.clear(0, 32, 64); // Dark blue background
    
    const ballSize = 8; // Larger ball for 128x128
    const ballX = Math.floor(Math.abs(Math.sin(frame * 0.1)) * (renderer.width - ballSize));
    const ballY = Math.floor(Math.abs(Math.cos(frame * 0.07)) * (renderer.height - ballSize));
    
    // Draw ball
    renderer.fillRect(ballX, ballY, ballSize, ballSize, 255, 255, 0);
    
    // Trail effect
    for (let i = 1; i < 8; i++) {
      const trailFrame = frame - i * 2;
      const trailX = Math.floor(Math.abs(Math.sin(trailFrame * 0.1)) * (renderer.width - ballSize));
      const trailY = Math.floor(Math.abs(Math.cos(trailFrame * 0.07)) * (renderer.height - ballSize));
      const intensity = 255 - (i * 30);
      
      if (intensity > 0) {
        renderer.fillRect(trailX, trailY, ballSize, ballSize, intensity, intensity, 0);
      }
    }
  }
};

// Performance benchmark
async function benchmark(patternName = 'colorBlocks', numFrames = 100) {
  const renderer = new BorderedSixelRenderer(BUFFER_WIDTH, BUFFER_HEIGHT, SCALE, BORDER_BLOCKS);
  const pattern = DemoPatterns[patternName];
  const frameTimes = [];
  
  console.log(`\nBenchmarking ${patternName} pattern (${numFrames} frames)...`);
  
  for (let frame = 0; frame < numFrames; frame++) {
    const frameStart = process.hrtime.bigint();
    
    // Generate frame
    const genStart = process.hrtime.bigint();
    pattern(renderer, frame);
    const genEnd = process.hrtime.bigint();
    
    // Render frame
    const renderStart = process.hrtime.bigint();
    const sixelData = renderer.render();
    process.stdout.write(sixelData);
    const renderEnd = process.hrtime.bigint();
    
    const frameEnd = process.hrtime.bigint();
    
    const totalTime = Number(frameEnd - frameStart) / 1000000;
    const genTime = Number(genEnd - genStart) / 1000000;
    const renderTime = Number(renderEnd - renderStart) / 1000000;
    
    frameTimes.push({
      total: totalTime,
      generation: genTime,
      render: renderTime,
      fps: 1000 / totalTime
    });
    
    // Progress indicator
    if ((frame + 1) % 25 === 0) {
      const avgFps = frameTimes.slice(-25).reduce((sum, f) => sum + f.fps, 0) / 25;
      process.stderr.write(`\rFrame ${frame + 1}/${numFrames} - Avg FPS: ${avgFps.toFixed(1)}`);
    }
  }
  
  // Calculate statistics
  const avgTotal = frameTimes.reduce((sum, f) => sum + f.total, 0) / numFrames;
  const avgGeneration = frameTimes.reduce((sum, f) => sum + f.generation, 0) / numFrames;
  const avgRender = frameTimes.reduce((sum, f) => sum + f.render, 0) / numFrames;
  const avgFps = 1000 / avgTotal;
  
  const minFps = Math.min(...frameTimes.map(f => f.fps));
  const maxFps = Math.max(...frameTimes.map(f => f.fps));
  
  const pixelsPerFrame = renderer.outputWidth * renderer.outputHeight;
  const contentPixels = renderer.contentWidth * renderer.contentHeight;
  const pixelsPerSecond = pixelsPerFrame * avgFps;
  
  // Clear and show results
  process.stdout.write('\x1b[2J\x1b[H');
  
  console.log('\n=== 128x128 BORDERED SIXEL RENDERER RESULTS ===');
  console.log(`Pattern: ${patternName}`);
  console.log(`Content: ${renderer.contentWidth}x${renderer.contentHeight} (${SCALE}x scaled from ${BUFFER_WIDTH}x${BUFFER_HEIGHT})`);
  console.log(`Output: ${renderer.outputWidth}x${renderer.outputHeight} (with ${BORDER_BLOCKS}×${SCALE}=${renderer.borderSize}px chunky border)`);
  console.log(`Frames: ${numFrames}`);
  console.log('');
  console.log('Performance:');
  console.log(`  Average FPS: ${avgFps.toFixed(1)}`);
  console.log(`  Min FPS: ${minFps.toFixed(1)}`);
  console.log(`  Max FPS: ${maxFps.toFixed(1)}`);
  console.log('');
  console.log('Timing Breakdown:');
  console.log(`  Generation: ${avgGeneration.toFixed(2)}ms (${(avgGeneration/avgTotal*100).toFixed(1)}%)`);
  console.log(`  Render: ${avgRender.toFixed(2)}ms (${(avgRender/avgTotal*100).toFixed(1)}%)`);
  console.log(`  Total: ${avgTotal.toFixed(2)}ms`);
  console.log('');
  console.log('Throughput:');
  console.log(`  Content pixels per frame: ${contentPixels.toLocaleString()}`);
  console.log(`  Total pixels per frame: ${pixelsPerFrame.toLocaleString()}`);
  console.log(`  Pixels per second: ${(pixelsPerSecond/1000000).toFixed(2)} megapixels/second`);
  console.log('');
  console.log('Method: Cursor save/restore with chunky black border');
  
  return { avgFps, pixelsPerSecond };
}

// Demo mode
async function demo(patternName = 'bouncingBall', duration = 10000) {
  const renderer = new BorderedSixelRenderer(BUFFER_WIDTH, BUFFER_HEIGHT, SCALE, BORDER_BLOCKS);
  const pattern = DemoPatterns[patternName];
  
  console.log(`\nRunning ${patternName} demo for ${duration}ms...`);
  console.log('Press Ctrl+C to stop\n');
  
  let frame = 0;
  const startTime = Date.now();
  
  const interval = setInterval(() => {
    pattern(renderer, frame++);
    const sixelData = renderer.render();
    process.stdout.write(sixelData);
    
    if (Date.now() - startTime > duration) {
      clearInterval(interval);
      process.stdout.write('\x1b[2J\x1b[H');
      console.log('Demo complete!');
    }
  }, 1000/30); // Target 30 FPS for demo
}

// CLI interface
if (process.argv.length > 2) {
  const command = process.argv[2];
  const pattern = process.argv[3] || 'bouncingBall';
  
  if (command === 'demo') {
    const duration = parseInt(process.argv[4]) || 10000;
    demo(pattern, duration);
  } else if (command === 'benchmark') {
    const frames = parseInt(process.argv[4]) || 100;
    benchmark(pattern, frames);
  } else {
    console.log('Usage:');
    console.log('  node bordered-64x64.mjs demo [pattern] [duration_ms]');
    console.log('  node bordered-64x64.mjs benchmark [pattern] [frames]');
    console.log('');
    console.log('Available patterns: colorBlocks, plasma, stripes, bouncingBall');
  }
} else {
  // Default benchmark
  benchmark().catch(console.error);
}

// Export for use as module
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { BorderedSixelRenderer, DemoPatterns, benchmark, demo };
}
