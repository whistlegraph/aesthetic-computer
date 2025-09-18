// Tape.mjs - Animation renderer for Aesthetic Computer
// Optimized for multi-frame sequences and time-based content

/* #region   constructor(width = 768, height = 768) {
    super(width, height);
    this.frames = []; // Keep metadata only
    this.frameCount = 0;
    this.sixelRenderer = new SixelRenderer(128, 128); // Fixed 128x128 sixel display
    
    // Disk-based frame caching for scalable rendering
    this.frameCacheDir = join(process.cwd(), '.tape-frames-cache');
    this.useDiskCache = true; // Always use disk cache for scalability
    this.setupFrameCache();
  }

  setupFrameCache() {
    if (this.useDiskCache) {
      // Create cache directory
      if (existsSync(this.frameCacheDir)) {
        rmSync(this.frameCacheDir, { recursive: true, force: true });
      }
      mkdirSync(this.frameCacheDir, { recursive: true });
      console.log(`📁 Frame cache directory: ${this.frameCacheDir}`);
    }
  }
}

/* #endregion */

import { HeadlessAC } from './headless.mjs';
import { timestamp } from "../../../system/public/aesthetic.computer/lib/num.mjs";
import chalk from 'chalk';
import boxen from 'boxen';
import figlet from 'figlet';
import readline from 'readline';
import { writeFileSync, readFileSync, mkdirSync, rmSync, existsSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { createCanvas } from 'canvas';
// Import gifenc for GIF export
import { GIFEncoder, quantize, applyPalette, prequantize } from '../../../system/public/aesthetic.computer/dep/gifenc/gifenc.esm.js';
// Import KidLisp for $code recording
import https from 'https';

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

  // Convert RGBA ImageData to RGB buffer
  fromImageData(imageData) {
    const sourceData = imageData.data;
    for (let i = 0; i < this.buffer.length / 3; i++) {
      const srcIdx = i * 4; // RGBA
      const dstIdx = i * 3; // RGB
      this.buffer[dstIdx] = sourceData[srcIdx];     // R
      this.buffer[dstIdx + 1] = sourceData[srcIdx + 1]; // G
      this.buffer[dstIdx + 2] = sourceData[srcIdx + 2]; // B
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

class Tape extends HeadlessAC {
  constructor(width = 128, height = 128, options = {}) {
    super(width, height, {
      detailedTiming: options.detailedTiming || false,
      useBlockProcessing: options.useBlockProcessing || false
    });
    this.frames = [];
    this.frameCount = 0;
    this.sixelRenderer = new SixelRenderer(128, 128); // Fixed 128x128 sixel display
    
    // Disk-based frame caching for scalable rendering
    this.frameCacheDir = join(process.cwd(), '.tape-frames-cache');
    this.useDiskCache = true; // Enable disk caching by default for scalability
    
    // Reuse single canvas for PNG generation to prevent memory bloat
    this.pngCanvas = null;
    this.pngCtx = null;
    
    this.setupFrameCache();
  }

  // Setup frame cache directory
  setupFrameCache() {
    if (this.useDiskCache) {
      if (existsSync(this.frameCacheDir)) {
        rmSync(this.frameCacheDir, { recursive: true, force: true });
      }
      mkdirSync(this.frameCacheDir, { recursive: true });
      console.log(`📁 Frame cache directory: ${this.frameCacheDir}`);
    }
  }

  cleanupFrameCache() {
    if (this.useDiskCache && existsSync(this.frameCacheDir)) {
      rmSync(this.frameCacheDir, { recursive: true, force: true });
      console.log(`🗑️ Cleaned up frame cache directory`);
    }
  }

  // Cache frame to disk as PNG only for memory efficiency and direct ffmpeg use
  cacheFrame(frameIndex, frameData, timestamp) {
    if (this.useDiskCache) {
      const framePngPath = join(this.frameCacheDir, `frame-${frameIndex.toString().padStart(6, '0')}.png`);
      
      // Memory monitoring for PNG generation debugging
      const beforeMem = process.memoryUsage();
      console.log(`Frame ${frameIndex} - Before PNG: RSS ${Math.round(beforeMem.rss / 1024 / 1024)}MB, Heap ${Math.round(beforeMem.heapUsed / 1024 / 1024)}MB`);
      
      // Save as PNG only - compressed, memory efficient, and directly usable by ffmpeg
      this.saveToPNG(frameData, framePngPath);
      
      const afterMem = process.memoryUsage();
      console.log(`Frame ${frameIndex} - After PNG: RSS ${Math.round(afterMem.rss / 1024 / 1024)}MB, Heap ${Math.round(afterMem.heapUsed / 1024 / 1024)}MB`);
      console.log(`Frame ${frameIndex} - Delta: RSS +${Math.round((afterMem.rss - beforeMem.rss) / 1024 / 1024)}MB, Heap +${Math.round((afterMem.heapUsed - beforeMem.heapUsed) / 1024 / 1024)}MB`);
      
      // Store metadata in memory
      this.frames.push({
        index: frameIndex,
        pngPath: framePngPath,
        timestamp: timestamp,
        width: this.width,
        height: this.height
      });
    } else {
      // Fallback to memory storage
      this.frames.push({
        data: new Uint8Array(frameData),
        timestamp: timestamp
      });
    }
  }

  // Convert RGBA data to PNG file efficiently with reused canvas and memory management
  saveToPNG(frameData, pngPath) {
    try {
      // Initialize reusable canvas if needed
      if (!this.pngCanvas) {
        this.pngCanvas = createCanvas(this.width, this.height);
        this.pngCtx = this.pngCanvas.getContext('2d');
        console.log(`📦 Initialized reusable PNG canvas: ${this.width}x${this.height}`);
      }
      
      // Reuse existing canvas and context - much more memory efficient
      const imageData = this.pngCtx.createImageData(this.width, this.height);
      imageData.data.set(frameData);
      this.pngCtx.putImageData(imageData, 0, 0);
      
      // Generate PNG buffer and save immediately
      const buffer = this.pngCanvas.toBuffer('image/png');
      writeFileSync(pngPath, buffer);
      
      // Force garbage collection hint for large buffers
      if (buffer.length > 1024 * 1024) { // > 1MB
        if (global.gc) {
          global.gc();
        }
      }
    } catch (error) {
      console.warn(`⚠️ Could not create PNG cache: ${error.message}`);
    }
  }

  // Read frame from disk cache (PNG format only)
  readCachedFrame(frameIndex) {
    if (this.useDiskCache) {
      const frameInfo = this.frames[frameIndex];
      if (frameInfo && frameInfo.pngPath) {
        // For PNG files, we could decode them back to RGBA if needed
        // For now, return null since most usage is direct PNG->ffmpeg
        console.warn('Reading PNG cache not yet implemented - consider using PNG path directly');
        return null;
      }
    } else {
      return this.frames[frameIndex]?.data;
    }
    return null;
  }

  // Scale frame to 128x128 and display as sixel in terminal
  displayFrameSixel(frameData, frameIndex, totalFrames) {
    try {
      // Scale down from current frame size to 128x128
      const sourceWidth = this.width;
      const sourceHeight = this.height;
      const targetWidth = 128;
      const targetHeight = 128;
      
      this.sixelRenderer.clear(0, 0, 0); // Clear to black
      
      // Handle different frame data formats consistently
      let pixelData;
      if (frameData instanceof Uint8Array) {
        pixelData = frameData;
      } else if (frameData && frameData.data instanceof Uint8Array) {
        pixelData = frameData.data;
      } else {
        console.log(`\n🎬 Frame ${frameIndex + 1}/${totalFrames} (invalid pixel data format)`);
        return;
      }
      
      // Validate pixel data size
      const expectedSize = sourceWidth * sourceHeight * 4; // RGBA
      if (pixelData.length < expectedSize) {
        console.log(`\n🎬 Frame ${frameIndex + 1}/${totalFrames} (pixel data too small: ${pixelData.length} < ${expectedSize})`);
        return;
      }
      
      // Improved nearest-neighbor scaling with bounds checking
      for (let y = 0; y < targetHeight; y++) {
        for (let x = 0; x < targetWidth; x++) {
          // Calculate source coordinates with bounds checking
          const sourceX = Math.min(Math.floor((x / targetWidth) * sourceWidth), sourceWidth - 1);
          const sourceY = Math.min(Math.floor((y / targetHeight) * sourceHeight), sourceHeight - 1);
          const sourceIdx = (sourceY * sourceWidth + sourceX) * 4; // RGBA
          
          // Extract RGB with bounds checking
          const r = sourceIdx < pixelData.length ? pixelData[sourceIdx] : 0;
          const g = sourceIdx + 1 < pixelData.length ? pixelData[sourceIdx + 1] : 0;
          const b = sourceIdx + 2 < pixelData.length ? pixelData[sourceIdx + 2] : 0;
          
          this.sixelRenderer.setPixel(x, y, r, g, b);
        }
      }
      
      // Output sixel
      const sixelData = this.sixelRenderer.render();
      console.log(`\n🎬 Frame ${frameIndex + 1}/${totalFrames}:`);
      process.stdout.write(sixelData);
      process.stdout.write('\n'); // Add newline
      
      // Force immediate output to terminal
      if (process.stdout.isTTY) {
        process.stdout.write('\x1b[0m'); // Reset terminal formatting
      }
      
    } catch (error) {
      console.log(`\n🎬 Frame ${frameIndex + 1}/${totalFrames} (sixel error: ${error.message})`);
    }
  }

  // Fetch KidLisp source code from the localhost API
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

  // Create enhanced API with real graph.mjs transformations
  createAPIWithRealTransformations() {
    console.log(`🔧 Creating enhanced API with real graph.mjs transformations...`);
    
    const baseAPI = this.createAPI();
    
    // Override transformation functions to use real graph.mjs implementations
    const enhancedAPI = {
      ...baseAPI,
      
      // Real spin transformation using graph.mjs
      spin: (amount = 0.1) => {
        console.log(`🔄 Real Spin: ${amount}`);
        if (this.graph && this.graph.spin) {
          return this.graph.spin(amount);
        }
        return baseAPI.spin(amount);
      },
      
            // Real zoom transformation using graph.mjs
      zoom: (factor = 1.1) => {
        console.log(`🔍 Real Zoom: ${factor}`);
        if (this.graph && this.graph.zoom) {
          // Debug: Check if buffer is properly set
          const buffer = this.graph.getBuffer ? this.graph.getBuffer() : { width: undefined, height: undefined };
          console.log(`🔧 Zoom Debug: Buffer ${buffer.width}x${buffer.height}, Factor: ${factor}`);
          
          // Call the zoom function
          const result = this.graph.zoom(factor);
          
          // Try to detect if zoom actually changed pixels
          const pixelSum = this.pixelBuffer.reduce((sum, val, i) => {
            if (i % 4 === 3) return sum; // Skip alpha channel
            return sum + val;
          }, 0);
          console.log(`� Zoom Applied: Pixel checksum: ${pixelSum}`);
          
          return result;
        }
        return baseAPI.zoom(factor);
      },
      
      // Real contrast transformation using graph.mjs
      contrast: (amount = 1.1) => {
        console.log(`🎨 Real Contrast: ${amount}`);
        if (this.graph && this.graph.contrast) {
          return this.graph.contrast(amount);
        }
        return baseAPI.contrast(amount);
      },
      
      // Real scroll transformation using graph.mjs
      scroll: (x = 0, y = 0) => {
        console.log(`📜 Real Scroll: ${x}, ${y}`);
        if (this.graph && this.graph.scroll) {
          return this.graph.scroll(x, y);
        }
        return baseAPI.scroll(x, y);
      }
    };
    
    console.log(`🔍 Enhanced API functions available:`, Object.keys(enhancedAPI).join(', '));
    return enhancedAPI;
  }

  // Record a KidLisp piece animation using proper disk.mjs system
  async recordKidLispPiece(code, duration = 3000, fps = 60, totalFrames = null) {
    console.log(`🎨 Recording KidLisp piece: ${code}`);
    
    // Fetch the KidLisp source code
    const source = await this.fetchKidLispSource(code);
    console.log(`📄 KidLisp source loaded (${source.length} chars)`);
    
    console.log(`🧠 Using proper disk.mjs system with KidLisp embedding for background fading`);
    
    // Create a temporary piece that uses the disk.mjs system with KidLisp
    return await this.recordWithDiskSystemEmbedded(code, source, duration, fps, totalFrames);
  }

  // Record using proper disk.mjs system with KidLisp embedding
  async recordWithDiskSystemEmbedded(code, source, duration, fps, totalFrames = null) {
    const framesText = totalFrames ? `${totalFrames} frames` : `duration: ${duration}ms`;
    console.log(`🎬 Recording via disk.mjs KidLisp system... (${framesText} @ ${fps}fps)`);
    
    // Initialize API first
    const api = this.createAPI();
    
    // Import the KidLisp system that disk.mjs uses
    const { KidLisp } = await import('../../../system/public/aesthetic.computer/lib/kidlisp.mjs');
    
    // Create a KidLisp instance like disk.mjs does
    const kidlispInstance = new KidLisp();
    
    // Initialize timer tracking systems for proper timer behavior
    kidlispInstance.lastSecondExecutions = {};
    kidlispInstance.onceExecuted = new Set();
    kidlispInstance.sequenceCounters = new Map();
    kidlispInstance.timingBlinks = new Map();
    
    // 🎨 PRE-POPULATE EMBEDDED SOURCE CACHE for proper rendering
    console.log('🎨 Pre-populating embedded source cache...');
    kidlispInstance.embeddedSourceCache = new Map();
    
    // Import the fetchCachedCode function to get real source
    const { fetchCachedCode, globalCodeCache } = await import('../../../system/public/aesthetic.computer/lib/kidlisp.mjs');
    
    // Fetch real source code for embedded pieces
    const realSource39i = await fetchCachedCode('39i');
    const realSourceR2f = await fetchCachedCode('r2f');
    
    if (realSource39i) {
      console.log('✅ Fetched real source for 39i:', realSource39i.substring(0, 50) + '...');
      kidlispInstance.embeddedSourceCache.set('39i', realSource39i);
      globalCodeCache.set('39i', realSource39i);
    } else {
      console.log('⚠️ Could not fetch real 39i source, using fallback');
      const fallback39i = `(fps 24)\n(wipe 255 100 100)\n(ink 255 255 255)\n(write "39i" 20 20)\n(box 10 10 60 60)\n(circle 40 40 25)`;
      kidlispInstance.embeddedSourceCache.set('39i', fallback39i);
      globalCodeCache.set('39i', fallback39i);
    }
    
    if (realSourceR2f) {
      console.log('✅ Fetched real source for r2f:', realSourceR2f.substring(0, 50) + '...');
      kidlispInstance.embeddedSourceCache.set('r2f', realSourceR2f);
      globalCodeCache.set('r2f', realSourceR2f);
    } else {
      console.log('⚠️ Could not fetch real r2f source, using fallback');
      const fallbackR2f = `(fps 24)\n(wipe 100 255 100)\n(ink 0 0 0)\n(write "r2f" 20 20)\n(circle 35 35 20)\n(box 15 50 40 30)`;
      kidlispInstance.embeddedSourceCache.set('r2f', fallbackR2f);
      globalCodeCache.set('r2f', fallbackR2f);
    }
    
    console.log('✅ Embedded cache populated with 39i and r2f in both instance and global cache');
    
    // Create the piece module for the KidLisp source
    const kidlispPiece = kidlispInstance.module(source, false);
    if (!kidlispPiece) {
      throw new Error('Failed to create KidLisp piece module');
    }
    
    // Use the standard recording system with the KidLisp piece
    const simFPS = fps; // Match simulation rate to paint rate for 1:1 correspondence
    const paintFPS = fps;
    const simRatio = 1; // Always 1 simulation step per frame for clean deterministic recording
    
    const calculatedFrames = totalFrames || Math.ceil((duration / 1000) * paintFPS);
    console.log(`🎬 Total frames to render: ${calculatedFrames}`);
    this.frameCount = 0;
    
    // Boot the piece
    // Create mock boot parameters to match disk.mjs expectations
    const mockBootParams = {
      wipe: function() { return api.wipe(0, 0, 0, 0); },
      backgroundFill: function(color) { 
        console.log(`🎨 Background fill with color: ${color}`);
        return api.wipe(color); // Actually apply the background color
      },
      params: [], // Add empty params array for KidLisp boot
      clock: { 
        time: () => new Date(this.simulationTime || Date.now()),
        resync: () => {} // Mock resync function
      },
      screen: {
        width: this.width,
        height: this.height
      },
      store: {
        persist: function(key, method = "local") {
          console.log(`📦 Mock store persist: ${key} (${method})`);
        },
        retrieve: function(key, method = "local") {
          console.log(`📦 Mock store retrieve: ${key} (${method})`);
          return Promise.resolve(null);
        },
        delete: function(key, method = "local") {
          console.log(`📦 Mock store delete: ${key} (${method})`);
          return Promise.resolve();
        }
      }
    };
    
    // Boot the piece with mock parameters
    if (kidlispPiece.boot) {
      kidlispPiece.boot(mockBootParams);
    }
    
    // Create enhanced API with store for KidLisp
    const kidlispAPI = {
      ...api,
      store: mockBootParams.store,
      backgroundFill: mockBootParams.backgroundFill, // Ensure backgroundFill is available
      clock: mockBootParams.clock // Ensure proper clock with simulation time
    };
    
    // CRITICAL: Set the API on the KidLisp instance so $code evaluation works
    // The embed function in $code evaluation uses globalEnv.embed which needs access to the API
    kidlispInstance.api = kidlispAPI;
    
    // Main recording loop using disk.mjs-style painting
    for (let frameIndex = 0; frameIndex < calculatedFrames; frameIndex++) {
      try {
        const frameStartTime = performance.now();
        
        // Calculate base frame time
        const baseFrameTime = (frameIndex / paintFPS) * 1000;
        
        // Don't clear screen - let disk.mjs background system handle it
        // this.graph.clear();
        
        // Run simulation steps for smooth animation with incremental timing
        const simStepsThisFrame = Math.round(simRatio);
        const simTimeStep = (1000 / simFPS); // Time per simulation step (e.g., 16.67ms for 60fps sim)
        
        for (let simStep = 0; simStep < simStepsThisFrame; simStep++) {
          try {
            // Calculate precise simulation time for this step
            const simulationTime = baseFrameTime + (simStep * simTimeStep);
            
            // COMPREHENSIVE TIME ISOLATION: Override global time functions before each frame
            const originalDateNow = Date.now;
            const originalPerfNow = performance.now;
            
            // Override global time functions to prevent KidLisp from accessing real system time
            Date.now = () => this.simulationTime;
            performance.now = () => this.simulationTime;
            
            // Set simulation time in API and ensure KidLisp API has updated clock
            this.setSimulationTime(simulationTime);
            kidlispAPI.clock.time = () => new Date(this.simulationTime || simulationTime);
            
            try {
              // Paint using the KidLisp piece with completely isolated timing
              if (kidlispPiece.paint) {
                kidlispPiece.paint(kidlispAPI);
              }
            } finally {
              // Restore original time functions
              Date.now = originalDateNow;
              performance.now = originalPerfNow;
            }
          } catch (evalError) {
            // Only show first few errors to avoid spam
            if (frameIndex < 5) {
              console.warn(`🚫 KidLisp paint warning at frame ${frameIndex}:`, evalError.message);
            }
          }
        }
        
        // Capture frame properly
        const frameData = this.captureFrame();
        this.cacheFrame(frameIndex, frameData, baseFrameTime);
        this.frameCount++;
        
        // Display frame as sixel in terminal
        this.displayFrameSixel(frameData, frameIndex, calculatedFrames);
        
        const frameEndTime = performance.now();
        const frameRenderTime = frameEndTime - frameStartTime;
        console.log(`⏱️ Frame ${frameIndex + 1}/${calculatedFrames} rendered in ${frameRenderTime.toFixed(1)}ms`);
        
      } catch (error) {
        console.error('\n💥 Error in KidLisp recording:', error);
        break;
      }
    }
    
    const actualFPS = (this.frameCount / (duration / 1000));
    console.log(`✅ KidLisp animation recorded • ${this.frameCount} frames • ${actualFPS.toFixed(1)} avg fps`);
    
    // Print performance summary
    this.printPerformanceSummary();
    
    // Note: Frame cache cleanup is deferred until after GIF export (if requested)
    
    return {
      success: true,
      timestamp: timestamp(),
      frames: this.frameCount,
      duration: duration,
      targetFPS: fps,
      actualFPS: actualFPS,
      apiCalls: this.frameCount * 10, // Estimate for KidLisp
      uniqueAPIs: 5,
      apis: ['wipe', 'ink', 'line', 'circle', 'kidlisp']
    };
  }

  // Record KidLisp animation frames using the enhanced API (old method)
  async recordAnimationFramesEnhancedAPI(kidlispInstance, source, duration, fps) {
    console.log(`🎬 Recording KidLisp animation... (sim: 60fps, paint: ${fps}fps)`);
    
    // Calculate timing - same deterministic approach as regular pieces
    const simFPS = 60; // Natural rate for KidLisp evaluation
    const paintFPS = fps;
    const simRatio = simFPS / paintFPS;
    
    const totalFrames = Math.ceil((duration / 1000) * paintFPS);
    this.frameCount = 0;
    
    // DETERMINISTIC LOOP for KidLisp
    for (let frameIndex = 0; frameIndex < totalFrames; frameIndex++) {
      try {
        const frameStartTime = performance.now();
        
        // Calculate deterministic timestamp
        const frameTime = (frameIndex / paintFPS) * 1000;
        
        // Inject deterministic time for this frame
        this.setSimulationTime(frameTime);
        
        // Clear screen for each frame
        this.graph.clear();
        
        // Run simulation steps
        const simStepsThisFrame = Math.round(simRatio);
        for (let simStep = 0; simStep < simStepsThisFrame; simStep++) {
          try {
            // Execute KidLisp with enhanced transformations
            kidlispInstance.parse(source);
            if (kidlispInstance.ast) {
              kidlispInstance.evaluate(kidlispInstance.ast, kidlispInstance.api, kidlispInstance.localEnv);
            }
          } catch (evalError) {
            // Only show first few errors to avoid spam
            if (frameIndex < 5) {
              console.warn(`🚫 KidLisp evaluation warning at frame ${frameIndex}:`, evalError.message);
            }
          }
        }
        
        // Capture frame properly
        const frameData = this.captureFrame();
        this.cacheFrame(frameIndex, frameData, frameTime);
        this.frameCount++;
        
        // Display frame as sixel in terminal
        this.displayFrameSixel(frameData, frameIndex, totalFrames);
        
        const frameEndTime = performance.now();
        const frameRenderTime = frameEndTime - frameStartTime;
        console.log(`⏱️ Frame ${frameIndex + 1}/${totalFrames} rendered in ${frameRenderTime.toFixed(1)}ms`);
        
      } catch (error) {
        console.error('\n💥 Error in KidLisp simulation:', error);
        break;
      }
    }
    
    const actualFPS = (this.frameCount / (duration / 1000));
    console.log(`✅ KidLisp animation recorded • ${this.frameCount} frames • ${actualFPS.toFixed(1)} avg fps`);
    
    // Note: Frame cache cleanup is deferred until after GIF export (if requested)
    
    return {
      success: true,
      timestamp: timestamp(),
      frames: this.frameCount,
      duration: duration,
      targetFPS: fps,
      actualFPS: actualFPS,
      apiCalls: this.frameCount * 10, // Estimate for KidLisp
      uniqueAPIs: 5,
      apis: ['wipe', 'ink', 'line', 'circle', 'kidlisp']
    };
  }

  async record(piecePath, duration = 3000, fps = 60, totalFrames = null) {
    // Check if this is a KidLisp $code
    if (piecePath.startsWith('$')) {
      return await this.recordKidLispPiece(piecePath, duration, fps, totalFrames);
    }
    
    const ts = timestamp();
    console.log(`🚀 Recording piece: ${piecePath} [${ts}]`);
    console.log(`📐 Canvas: ${this.width}x${this.height}, Duration: ${duration}ms @ ${fps}fps`);
    
    try {
      // Load the piece module directly to access sim and paint separately
      const pieceModule = await this.loadPieceModule(piecePath);
      
      // Create API  
      const api = this.createAPI();
      
      // Initialize if it's an AC disk
      if (pieceModule.boot) {
        pieceModule.boot(api);
      }
      
      // Calculate timing - DETERMINISTIC FRAME-BASED APPROACH
      const simFPS = 60; // Match simulation to paint rate for deterministic timing
      const paintFPS = fps; // Recording frame rate
      
      console.log(`🎬 Recording animation... (sim: ${simFPS}fps, paint: ${paintFPS}fps)`);
      
      // Calculate total frames needed - use explicit totalFrames if provided
      const actualTotalFrames = totalFrames || Math.ceil((duration / 1000) * paintFPS);
      let progressUpdateCount = 0;
      this.frameCount = 0;
      
      console.log(`📊 Recording ${actualTotalFrames} frames (${totalFrames ? 'explicit count' : 'calculated from duration'})`);
      
      // Track deterministic simulation time
      let simulationTime = 0; // Start at time 0
      const frameTimeStep = 1000 / paintFPS; // Time per frame in ms
      
      // DETERMINISTIC LOOP: Frame-based instead of time-based
      for (let frameIndex = 0; frameIndex < actualTotalFrames; frameIndex++) {
        try {
          // Calculate deterministic timestamp for this frame
          const frameTime = (frameIndex / paintFPS) * 1000; // Exact timestamp in ms
          
          // Update simulation time for this frame (this drives the simulation)
          simulationTime = frameTime;
          
          // Inject deterministic time into the API for timeless rendering
          this.setSimulationTime(simulationTime);
          
          // Single simulation step per frame at deterministic time
          if (pieceModule.sim) {
            pieceModule.sim(api);
          }
          
          // Clear API calls for this frame
          this.apiCalls = [];
          
          // Render frame
          pieceModule.paint(api);
          this.frameCount++;
          
          // Store frame data for GIF/video export with exact timestamp
          this.cacheFrame(frameIndex, new Uint8Array(this.pixelBuffer), frameTime);
          
          // Display frame as sixel in terminal
          const frameData = { data: this.pixelBuffer };
          this.displayFrameSixel(frameData, frameIndex, actualTotalFrames);
          
        } catch (error) {
          console.error('\n💥 Error in simulation/paint:', error);
          break;
        }
      }
      
      const actualFPS = (this.frameCount / (duration / 1000));
      const totalAPICalls = this.frameCount * (this.apiCalls?.length || 0);
      const stats = this.getStats();
      
      console.log(`✅ Animation recorded • ${this.frameCount} frames • ${actualFPS.toFixed(1)} avg fps`);
      
      // Note: Frame cache cleanup is deferred until after GIF export (if requested)
      
      return {
        success: true,
        timestamp: ts,
        frames: this.frameCount,
        duration: duration,
        targetFPS: fps,
        actualFPS: actualFPS,
        apiCalls: totalAPICalls,
        uniqueAPIs: stats.uniqueAPIs,
        apis: stats.apis
      };
      
    } catch (error) {
      console.error('💥 Error recording piece:', error);
      return {
        success: false,
        error: error.message,
        timestamp: ts,
        frames: this.frameCount,
        apiCalls: 0,
        uniqueAPIs: 0,
        apis: []
      };
    }
  }

  // TODO: Implement frame capture for GIF/video export
  captureFrame() {
    // Return copy of current pixel buffer
    return new Uint8Array(this.pixelBuffer);
  }

  // Real GIF export implementation using gifenc (BIOS-style)
  async exportGIF(filename, gifFPS = 60) {
    if (this.frames.length === 0) {
      throw new Error('No frames to export - record animation first');
    }

    const outputPath = filename; // filename is already a full path
    console.log(`🎞️ Exporting ${this.frames.length} frames to ${filename} @ ${gifFPS}fps...`);
    
    // Calculate frame delay based on target GIF FPS (like BIOS: 60fps = 16.67ms)
    const frameDelayMs = Math.round(1000 / gifFPS); // e.g., 60fps = 16.67ms → 17ms
    const actualFPS = 1000 / frameDelayMs; // Calculate actual FPS from milliseconds
    console.log(`⏱️ Frame delay: ${frameDelayMs}ms = ${actualFPS.toFixed(1)}fps (target: ${gifFPS}fps)`);

    try {
      // ULTRA-MEMORY-EFFICIENT APPROACH: Downsample and pixel-sample for palette
      console.log('🎨 Generating color palette from downsampled frame samples...');
      
      // Sample frames for palette generation (use every Nth frame)
      const maxSampleFrames = Math.min(5, this.frameCount); // Even fewer sample frames
      const sampleStep = Math.max(1, Math.floor(this.frameCount / maxSampleFrames));
      
      // Downsample resolution for palette generation to reduce memory usage
      const paletteWidth = Math.min(256, this.width);   // Max 256x256 for palette
      const paletteHeight = Math.min(256, this.height);
      const pixelStep = 4; // Sample every 4th pixel within the downsampled frame
      
      console.log(`📐 Using ${paletteWidth}x${paletteHeight} downsampled frames for palette (${maxSampleFrames} samples)`);
      
      const samplePixels = [];
      
      for (let i = 0; i < this.frameCount; i += sampleStep) {
        const frameData = this.readCachedFrame(i);
        
        // Create much smaller sample for palette generation
        const sampleSize = Math.floor((paletteWidth * paletteHeight) / (pixelStep * pixelStep)) * 4;
        const sampleData = new Uint8ClampedArray(sampleSize);
        let sampleIndex = 0;
        
        // Downsample and pixel-sample the frame
        for (let y = 0; y < paletteHeight && sampleIndex < sampleSize - 4; y += pixelStep) {
          for (let x = 0; x < paletteWidth && sampleIndex < sampleSize - 4; x += pixelStep) {
            // Map downsampled coordinates to original frame coordinates
            const origX = Math.floor((x / paletteWidth) * this.width);
            const origY = Math.floor((y / paletteHeight) * this.height);
            const origIndex = (origY * this.width + origX) * 4;
            
            // Copy RGBA values if within bounds
            if (origIndex + 3 < frameData.length) {
              sampleData[sampleIndex] = frameData[origIndex] || 0;     // R
              sampleData[sampleIndex + 1] = frameData[origIndex + 1] || 0; // G
              sampleData[sampleIndex + 2] = frameData[origIndex + 2] || 0; // B
              sampleData[sampleIndex + 3] = frameData[origIndex + 3] || 255; // A
              sampleIndex += 4;
            }
          }
        }
        
        // Trim to actual used size
        const trimmedSample = new Uint8ClampedArray(sampleIndex);
        trimmedSample.set(sampleData.subarray(0, sampleIndex));
        samplePixels.push(trimmedSample);
        
        console.log(`📊 Sampled frame ${i + 1}/${this.frameCount} (${(sampleIndex/4)} pixels from ${this.width}x${this.height})`);
      }
      
      // Create consolidated sample data for quantization
      const totalSamplePixels = samplePixels.reduce((sum, sample) => sum + sample.length, 0);
      const allSamplePixels = new Uint8ClampedArray(totalSamplePixels);
      let offset = 0;
      
      for (const sample of samplePixels) {
        allSamplePixels.set(sample, offset);
        offset += sample.length;
      }
      
      console.log(`🎨 Total palette sample data: ${(totalSamplePixels / (1024 * 1024)).toFixed(1)}MB`);
      
      // Apply prequantization to sample data
      console.log('🎨 Applying prequantization to sample data...');
      prequantize(allSamplePixels, {
        roundRGB: 5,      // More aggressive rounding for smaller sample set
        roundAlpha: 20,   // More aggressive alpha rounding
        oneBitAlpha: null // Keep full alpha channel
      });
      
      // Enhanced quantization with better quality settings
      const palette = quantize(allSamplePixels, 256, { 
        format: "rgba4444", // Better color precision
        clearAlpha: false,
        oneBitAlpha: false,
        colorSpace: "rgb"
      });
      
      console.log(`🎨 Created palette with ${palette.length} colors from ${(totalSamplePixels/4)} sample pixels`);

      // Create GIF encoder with conservative settings
      const gif = GIFEncoder({
        auto: true,
        initialCapacity: Math.max(4096, this.frameCount * 1024) // Much smaller initial capacity
      });
      
      // STREAMING PROCESSING: Convert each frame individually
      console.log('🎨 Processing frames individually for GIF...');
      for (let i = 0; i < this.frameCount; i++) {
        // Read one frame at a time
        const frameData = this.readCachedFrame(i);
        
        // Process frame in chunks to avoid large memory allocation
        const chunkSize = this.width * 100 * 4; // Process 100 rows at a time
        const rgbaData = new Uint8ClampedArray(this.width * this.height * 4);
        
        for (let chunk = 0; chunk < frameData.length; chunk += chunkSize) {
          const chunkEnd = Math.min(chunk + chunkSize, frameData.length);
          const chunkData = frameData.subarray(chunk, chunkEnd);
          
          // Copy chunk and ensure alpha channel
          for (let j = 0; j < chunkData.length; j += 4) {
            rgbaData[chunk + j] = chunkData[j] || 0;       // R
            rgbaData[chunk + j + 1] = chunkData[j + 1] || 0; // G
            rgbaData[chunk + j + 2] = chunkData[j + 2] || 0; // B
            rgbaData[chunk + j + 3] = chunkData[j + 3] || 255; // A
          }
        }
        
        // Apply palette
        const indexedData = applyPalette(rgbaData, palette, "rgba4444");
        
        // Write frame to GIF
        if (i === 0) {
          gif.writeFrame(indexedData, this.width, this.height, {
            palette: palette,
            delay: frameDelayMs,
            repeat: 0,
            transparent: false,
            dispose: -1
          });
        } else {
          gif.writeFrame(indexedData, this.width, this.height, {
            delay: frameDelayMs,
            transparent: false,
            dispose: -1
          });
        }
        
        // Progress reporting
        if (i % 5 === 0 || i === this.frameCount - 1) {
          console.log(`🖼️ Processed frame ${i + 1}/${this.frameCount}`);
        }
        
        // Force garbage collection hint
        if (global.gc && i % 10 === 0) {
          global.gc();
        }
      }
      
      const gifBuffer = gif.bytes();
      
      writeFileSync(outputPath, gifBuffer);
      
      console.log('✅ GIF exported successfully!');
      console.log(`📁 Output: ${outputPath}`);
      console.log(`📊 File size: ${(gifBuffer.length / 1024).toFixed(1)} KB`);
      
      // Clean up frame cache after successful export
      if (this.useDiskCache) {
        this.cleanupFrameCache();
      }
      
      return outputPath;
      
    } catch (error) {
      console.error('💥 Error exporting GIF:', error);
      throw error;
    }
  }

  // MP4 export implementation using ffmpeg - optimized for social media
  async exportMP4(filename, mp4FPS = 60, quality = 'instagram') {
    if (this.frames.length === 0) {
      throw new Error('No frames to export - record animation first');
    }

    const outputPath = filename; // filename is already a full path
    console.log(`🎬 Exporting ${this.frames.length} frames to ${filename} @ ${mp4FPS}fps (${quality} quality)...`);
    
    // Social media optimized settings
    const qualitySettings = {
      instagram: { 
        crf: 20, preset: 'medium', profile: 'main', 
        maxrate: '3500k', bufsize: '7000k',
        desc: 'Instagram optimized'
      },
      twitter: { 
        crf: 22, preset: 'medium', profile: 'main', 
        maxrate: '2500k', bufsize: '5000k',
        desc: 'Twitter optimized'
      },
      studio: { 
        crf: 15, preset: 'slower', profile: 'high',
        desc: 'Studio quality (highest)'
      },
      high: { 
        crf: 18, preset: 'slow', profile: 'high',
        desc: 'High quality'
      },
      medium: { 
        crf: 23, preset: 'medium', profile: 'main',
        desc: 'Medium quality'
      },
      low: { 
        crf: 28, preset: 'fast', profile: 'baseline',
        desc: 'Low quality/fast encode'
      }
    };
    
    const settings = qualitySettings[quality] || qualitySettings.studio;
    console.log(`🎯 Using ${settings.desc} settings`);
    
    try {
      // Check if we have cached PNG files to use directly
      const hasCachedPNGs = this.useDiskCache && this.frames.length > 0 && 
                            this.frames[0].pngPath && existsSync(this.frames[0].pngPath);
      
      let tempDir;
      if (hasCachedPNGs) {
        // Use existing cache directory with PNG files
        tempDir = this.frameCacheDir;
        console.log(`📁 Using cached PNG frames from ${tempDir}...`);
        console.log(`🚀 Memory-efficient export: Using ${this.frameCount} pre-cached PNG frames`);
      } else {
        // Fallback: Create temporary PNG files from frame data
        tempDir = '/tmp/tape-frames';
        const { execSync } = await import('child_process');
        
        // Clean up any existing temp directory
        try {
          execSync(`rm -rf ${tempDir}`, { stdio: 'pipe' });
        } catch (e) {
          // Ignore if directory doesn't exist
        }
        
        execSync(`mkdir -p ${tempDir}`, { stdio: 'pipe' });
        console.log(`📁 Creating temporary frames in ${tempDir}...`);
        
        // Export frames as individual PNG files with memory optimization
        const { writeFileSync } = await import('fs');
        
        for (let i = 0; i < this.frameCount; i++) {
          const frameData = this.readCachedFrame(i);
          
          // Convert RGBA to PNG using canvas (more memory efficient than sharp for large images)
          const frameFilename = `${tempDir}/frame-${i.toString().padStart(6, '0')}.png`;
          
          const { createCanvas } = await import('canvas');
          const canvas = createCanvas(this.width, this.height);
          const ctx = canvas.getContext('2d');
          
          const imageData = ctx.createImageData(this.width, this.height);
          imageData.data.set(frameData);
          ctx.putImageData(imageData, 0, 0);
          
          const buffer = canvas.toBuffer('image/png');
          writeFileSync(frameFilename, buffer);
          
          // Force garbage collection every 10 frames for large images
          if (i % 10 === 0 && global.gc) {
            global.gc();
          }
          
          // Progress reporting
          if (i % 5 === 0 || i === this.frameCount - 1) {
            console.log(`🖼️ Exported frame ${i + 1}/${this.frameCount}`);
          }
        }
      }
      
      console.log(`🎬 Converting ${this.frameCount} frames to MP4 with ffmpeg...`);
      
      // Build ffmpeg command with social media optimization
      const { execSync } = await import('child_process');
      const ffmpegArgs = [
        'ffmpeg', '-y',  // Overwrite output
        '-framerate', mp4FPS.toString(),
        '-i', `${tempDir}/frame-%06d.png`,
        '-c:v', 'libopenh264',  // H.264 for best compatibility (using libopenh264 codec)
        '-pix_fmt', 'yuv420p',  // Required for social media compatibility
        '-movflags', '+faststart',  // Optimize for web streaming
        '-r', mp4FPS.toString()  // Output framerate
      ];
      
      // Add quality settings (libopenh264 supports different parameters than libx264)
      if (quality === 'instagram' || quality === 'twitter') {
        ffmpegArgs.push('-b:v', '2M');  // Target bitrate for social media
      } else if (quality === 'studio') {
        ffmpegArgs.push('-b:v', '32M');  // Ultra high bitrate for studio quality
      } else {
        ffmpegArgs.push('-b:v', '4M');  // Higher bitrate for other quality levels
      }
      
      // Add bitrate limiting for social media platforms
      if (settings.maxrate) {
        ffmpegArgs.push('-maxrate', settings.maxrate);
        ffmpegArgs.push('-bufsize', settings.bufsize);
      }
      
      // Add color space settings for better compatibility
      ffmpegArgs.push('-colorspace', 'bt709');
      ffmpegArgs.push('-color_primaries', 'bt709');
      ffmpegArgs.push('-color_trc', 'bt709');
      
      ffmpegArgs.push(outputPath);
      
      const ffmpegCmd = ffmpegArgs.join(' ');
      console.log(`🔧 Running: ${ffmpegCmd}`);
      
      try {
        execSync(ffmpegCmd, { stdio: 'pipe' });
        console.log(`✅ Successfully encoded with libopenh264`);
      } catch (error) {
        // Try mpeg4 as fallback
        console.log(`❌ libopenh264 failed, trying mpeg4...`);
        const basicCmd = [
          'ffmpeg', '-y',
          '-framerate', mp4FPS.toString(),
          '-i', `${tempDir}/frame-%06d.png`,
          '-c:v', 'mpeg4',
          '-pix_fmt', 'yuv420p',
          '-b:v', '3M',
          outputPath
        ].join(' ');
        
        try {
          execSync(basicCmd, { stdio: 'pipe' });
          console.log(`✅ Successfully encoded with mpeg4`);
        } catch (error2) {
          // Final fallback to libvpx (WebM format)
          console.log(`❌ mpeg4 failed, trying libvpx_vp8 (WebM)...`);
          const webmPath = outputPath.replace('.mp4', '.webm');
          const webmCmd = [
            'ffmpeg', '-y',
            '-framerate', mp4FPS.toString(),
            '-i', `${tempDir}/frame-%06d.png`,
            '-c:v', 'libvpx_vp8',
            '-b:v', '2M',
            webmPath
          ].join(' ');
          
          execSync(webmCmd, { stdio: 'pipe' });
          console.log(`✅ Successfully encoded as WebM: ${webmPath}`);
          
          // Update the output path for the final reporting
          outputPath = webmPath;
        }
      }

      // Clean up temporary files only if we created them
      if (!hasCachedPNGs) {
        console.log('🧹 Cleaning up temporary frames...');
        const { execSync } = await import('child_process');
        execSync(`rm -rf ${tempDir}`, { stdio: 'pipe' });
      } else {
        console.log('💾 Keeping cached PNG frames for future use...');
      }
      
      // Get file size
      const { statSync } = await import('fs');
      const stats = statSync(outputPath);
      
      console.log('✅ MP4 exported successfully!');
      console.log(`📁 Output: ${outputPath}`);
      console.log(`📊 File size: ${(stats.size / (1024 * 1024)).toFixed(1)} MB`);
      
      // Clean up frame cache after successful export
      if (this.useDiskCache) {
        this.cleanupFrameCache();
      }
      
      return outputPath;
      
    } catch (error) {
      console.error('💥 Error exporting MP4:', error);
      throw error;
    }
  }

  // Capture a single frame at a specific time/frame index
  async captureSingleFrame(piecePath, frameIndex = 0, fps = 60, totalFrames = null) {
    console.log(`🖼️ Capturing single frame ${frameIndex} from piece: ${piecePath}`);
    
    try {
      // For static pieces (non-animated), just render once
      if (!piecePath.startsWith('$')) {
        console.log('🎨 Rendering static piece...');
        const paintFn = await this.loadPiece(piecePath);
        const api = this.createAPI();
        await paintFn(api);
        
        const frameData = this.captureFrame();
        console.log(`✅ Static frame captured`);
        
        return {
          success: true,
          frames: 1,
          timestamp: timestamp(),
          apiCalls: this.apiCalls.length,
          uniqueAPIs: [...new Set(this.apiCalls.map(call => call.name))].length,
          apis: [...new Set(this.apiCalls.map(call => call.name))]
        };
      }
      
      // For KidLisp pieces, simulate animation to the desired frame
      console.log(`🎬 Simulating animation to frame ${frameIndex}...`);
      
      // Calculate timing for the specific frame
      const frameTime = (frameIndex / fps) * 1000;
      const duration = frameTime + (1000 / fps); // Simulate just enough to reach the frame
      
      // Record just enough frames to reach the target frame
      const result = await this.record(piecePath, duration, fps, frameIndex + 1);
      
      if (result.success && this.frameCount > frameIndex) {
        console.log(`✅ Frame ${frameIndex} captured from animation`);
        
        // Load the specific frame from cache
        const frameData = this.readCachedFrame(frameIndex);
        if (frameData) {
          // Update pixel buffer with the captured frame
          this.pixelBuffer = new Uint8Array(frameData);
          return {
            success: true,
            frames: 1,
            capturedFrame: frameIndex,
            timestamp: timestamp(),
            apiCalls: result.apiCalls,
            uniqueAPIs: result.uniqueAPIs,
            apis: result.apis
          };
        }
      }
      
      throw new Error(`Failed to capture frame ${frameIndex}`);
      
    } catch (error) {
      console.error('💥 Error capturing frame:', error);
      return {
        success: false,
        error: error.message,
        frames: 0,
        timestamp: timestamp(),
        apiCalls: 0,
        uniqueAPIs: 0,
        apis: []
      };
    }
  }

  // TODO: Implement video export
  async exportVideo(filename, format = 'mp4', frameRate = 30) {
    console.log(`🎥 TODO: Export ${this.frameCount} frames to ${filename} (${format}) @ ${frameRate}fps`);
    // Implementation for video export will go here
  }
}

// Export the Tape class for programmatic use
export default Tape;

// CLI interface
if (import.meta.url === `file://${process.argv[1]}`) {
  const args = process.argv.slice(2);
  let piecePath = args[0];
  
  // Parse flags
  const getArgValue = (flag) => {
    const index = args.indexOf(flag);
    return index !== -1 && index + 1 < args.length ? args[index + 1] : null;
  };
  
  const framesArg = getArgValue('--frames');
  const fpsArg = getArgValue('--fps');
  const resolutionArg = getArgValue('--resolution');
  const frameArg = getArgValue('--frame'); // Single frame capture
  const widthArg = getArgValue('--width');
  const heightArg = getArgValue('--height');
  const qualityArg = getArgValue('--quality'); // MP4 quality setting
  const shouldExportPng = args.includes('--png'); // PNG is now optional
  const shouldExportGif = args.includes('--gif') || (!args.includes('--png') && !args.includes('--mp4')); // GIF is default unless --png or --mp4 specified
  const shouldExportMp4 = args.includes('--mp4'); // MP4 export option
  const detailedTiming = args.includes('--timing'); // Add timing flag
  const useBlockProcessing = args.includes('--blocks'); // 🧪 EXPERIMENTAL: Block-based processing
  const singleFrameMode = frameArg !== null; // Single frame capture mode
  
  // Calculate duration and fps
  let duration = 1000; // default 1 second
  let fps = 60; // default fps - smooth 60fps animations by default!
  let resolution = 768; // default resolution
  let width = 768; // default width
  let height = 768; // default height
  let requestedFrames = null; // Track if frames were explicitly requested
  let captureFrame = 0; // Default to first frame for single frame capture
  
  // Handle custom resolution and dimensions
  if (resolutionArg) {
    resolution = parseInt(resolutionArg);
    width = height = resolution; // Square by default
  }
  
  if (widthArg) {
    width = parseInt(widthArg);
  }
  
  if (heightArg) {
    height = parseInt(heightArg);
  }
  
  if (frameArg) {
    captureFrame = parseInt(frameArg);
  }
  
  // Handle MP4 quality setting
  let mp4Quality = 'studio'; // Default to studio quality for highest bitrate
  if (qualityArg) {
    const validQualities = ['instagram', 'twitter', 'studio', 'high', 'medium', 'low'];
    if (validQualities.includes(qualityArg)) {
      mp4Quality = qualityArg;
    } else {
      console.warn(`⚠️ Invalid quality "${qualityArg}". Using "studio". Valid options: ${validQualities.join(', ')}`);
    }
  }
  
  if (framesArg && fpsArg) {
    requestedFrames = parseInt(framesArg);
    fps = parseInt(fpsArg);
    // Don't calculate duration from frames - let KidLisp pieces run for their natural duration
    // duration = Math.ceil((requestedFrames / fps) * 1000); // This makes animations too fast!
  } else if (framesArg) {
    requestedFrames = parseInt(framesArg);
    // Don't calculate duration from frames - use default 1000ms
    // duration = Math.ceil((requestedFrames / fps) * 1000); // This makes animations too fast!
  } else if (fpsArg) {
    fps = parseInt(fpsArg);
  } else {
    // Fallback to positional arguments for backwards compatibility
    // If just a number is provided, treat it as frame count (not duration)
    if (args[1] && !args[2]) {
      // Single numeric argument = frame count (maintain 60fps)
      requestedFrames = parseInt(args[1]);
      fps = 60; // Keep default 60fps
    } else {
      // Legacy mode: duration (ms) and fps
      duration = parseInt(args[1]) || 1000;
      fps = parseInt(args[2]) || 60; // 60fps default for legacy mode too
    }
  }
  
  if (!piecePath) {
    console.error(chalk.red('Usage: node tape.mjs <piece-path> [OPTIONS]'));
    console.log(chalk.gray('\nAnimation mode:'));
    console.log(chalk.gray('  node tape.mjs <piece> [--frames N] [--fps N] [--resolution N] [--png|--gif|--mp4] [--quality instagram|twitter|studio|high|medium|low] [--timing] [--blocks]'));
    console.log(chalk.gray('\nSingle frame mode:'));
    console.log(chalk.gray('  node tape.mjs <piece> --frame N [--width W] [--height H] [--resolution N]'));
    console.log(chalk.gray('\nExamples:'));
    console.log(chalk.gray('  node tape.mjs $cow 240  # 240 frames at 60fps'));
    console.log(chalk.gray('  node tape.mjs $cow --frames 10 --resolution 1024 --timing'));
    console.log(chalk.gray('  node tape.mjs $rose --png  # Export PNG instead of default GIF'));
    console.log(chalk.gray('  node tape.mjs elcid-flyer.mjs --mp4 --frames 60 --quality instagram  # Export MP4 for Instagram'));
    console.log(chalk.gray('  node tape.mjs elcid-flyer.mjs --mp4 --quality twitter  # Export MP4 for Twitter'));
    console.log(chalk.gray('  node tape.mjs $piece --fps 30  # Override default 60fps'));
    console.log(chalk.gray('  node tape.mjs elcid-flyer.mjs --frame 0 --width 1920 --height 1080  # High-res flyer'));
    console.log(chalk.gray('  node tape.mjs $roz --frame 30 --resolution 2048  # Capture 30th frame at 2048x2048'));
    console.log(chalk.gray('  node tape.mjs $roz --blocks  # 🧪 Experimental block-based processing'));
    console.log(chalk.gray('  node tape.mjs $roz --simd    # 🚀 Experimental SIMD processing'));
    console.log(chalk.gray('\nLegacy: node tape.mjs test-complex.mjs 2000 30  # 2000ms duration, 30fps'));
    process.exit(1);
  }

  // Auto-prefix with tapes folder if just filename given
  if (!piecePath.includes('/') && !piecePath.startsWith('../')) {
    const tapesPath = `../test-pieces/tapes/${piecePath}`;
    // Check if file exists in tapes folder
    try {
      await import(tapesPath);
      piecePath = tapesPath;
    } catch {
      // Keep original path if not found in tapes folder
    }
  }
  
  console.log(chalk.blue(`🎬 Target piece: ${piecePath}`));
  
  if (singleFrameMode) {
    console.log(chalk.blue(`🖼️  Single frame capture: frame ${captureFrame}`));
    console.log(chalk.blue(`📐 Resolution: ${width}x${height}\n`));
  } else {
    console.log(chalk.blue(`⏱️  Duration: ${duration}ms @ ${fps}fps`));
    console.log(chalk.blue(`📐 Resolution: ${resolution}x${resolution}\n`));
  }
  
  try {
    // Create tape recorder with specified dimensions
    const tape = new Tape(width, height, { 
      detailedTiming,
      useBlockProcessing
    });
    
    // Initialize AC system
    await tape.initializeAC();
    
    let result;
    
    if (singleFrameMode) {
      // Single frame capture mode
      result = await tape.captureSingleFrame(piecePath, captureFrame, fps, requestedFrames);
    } else {
      // Animation recording mode
      result = await tape.record(piecePath, duration, fps, requestedFrames);
    }
    
    // Handle exports
    let gifPath = null;
    let pngPath = null;
    let mp4Path = null;
    
    if (singleFrameMode || shouldExportPng) {
      // Always export PNG for single frame mode, or when --png is specified
      console.log('\n' + chalk.blue('🖼️ Exporting PNG...'));
      const saveResult = tape.savePNG('/workspaces/aesthetic-computer/reference/tools/output/tape');
      pngPath = saveResult.filename;
    }
    
    if (!singleFrameMode && shouldExportGif && result.success) {
      // Export GIF for animation mode (unless single frame)
      console.log('\n' + chalk.blue('🎞️ Exporting GIF...'));
      const timestamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, 19);
      const gifFilename = `/workspaces/aesthetic-computer/reference/tools/output/tape-${timestamp}.gif`;
      gifPath = await tape.exportGIF(gifFilename, fps);
    }

    if (!singleFrameMode && shouldExportMp4 && result.success) {
      // Export MP4 for animation mode (unless single frame)
      console.log('\n' + chalk.blue('🎬 Exporting MP4...'));
      const timestamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, 19);
      const mp4Filename = `/workspaces/aesthetic-computer/reference/tools/output/tape-${timestamp}.mp4`;
      mp4Path = await tape.exportMP4(mp4Filename, fps, mp4Quality);
    }
    
    // Clean up frame cache (both formats handle their own cleanup)
    if (tape.useDiskCache) {
      tape.cleanupFrameCache();
    }
    
    // Final summary
    let summaryLines = [
      `${chalk.cyan('Success:')} ${result.success}`,
      `${chalk.cyan('Frames:')} ${result.frames}`,
    ];
    
    if (singleFrameMode) {
      summaryLines.push(`${chalk.cyan('Captured Frame:')} ${result.capturedFrame || captureFrame}`);
      summaryLines.push(`${chalk.cyan('Resolution:')} ${width}x${height}`);
    } else {
      summaryLines.push(`${chalk.cyan('Duration:')} ${result.duration}ms`);
      summaryLines.push(`${chalk.cyan('Target FPS:')} ${result.targetFPS}`);
      summaryLines.push(`${chalk.cyan('Actual FPS:')} ${result.actualFPS?.toFixed(1) || 'N/A'}`);
    }
    
    summaryLines.push(`${chalk.cyan('API Calls:')} ${result.apiCalls}`);
    summaryLines.push(`${chalk.cyan('Unique APIs:')} ${result.uniqueAPIs}`);
    summaryLines.push(`${chalk.cyan('APIs Used:')} ${result.apis.join(', ') || 'none'}`);
    
    if (gifPath) {
      summaryLines.push(`${chalk.cyan('GIF Export:')} ${gifPath}`);
    }
    
    if (mp4Path) {
      summaryLines.push(`${chalk.cyan('MP4 Export:')} ${mp4Path}`);
    }
    
    if (pngPath) {
      summaryLines.push(`${chalk.cyan('PNG Export:')} ${pngPath}`);
    }
    
    const modeText = singleFrameMode ? 'Frame Capture' : 'Recording';
    const summary = `${chalk.green(`✅ ${modeText} Complete!`)}\n\n` + summaryLines.join('\n');
    
    const boxedSummary = boxen(summary, { 
      padding: 1, 
      borderColor: 'green',
      borderStyle: 'round'
    });
    
    console.log('\n' + boxedSummary);
    
    // Show appropriate TODO message based on mode
    if (!singleFrameMode) {
      console.log(chalk.yellow('\n🚧 Coming Soon:'));
      console.log(chalk.gray('   • Audio integration with speaker.mjs'));
      console.log(chalk.gray('   • Synth.mjs support for musical pieces'));
      console.log(chalk.gray('   • Advanced video codecs (AV1, HEVC)'));
    } else {
      console.log(chalk.yellow('\n💡 Pro Tips:'));
      console.log(chalk.gray('   • Use --width and --height for custom aspect ratios'));
      console.log(chalk.gray('   • Combine with animation: --frame 30 to capture mid-animation'));
      console.log(chalk.gray('   • High-res flyers: --resolution 2048 or --width 1920 --height 1080'));
    }
    
    if (!result.success) {
      console.log(`\n${chalk.red('💥 Error:')} ${result.error}`);
      process.exit(1);
    }
    
    // Force exit to prevent hanging
    setTimeout(() => {
      console.log(chalk.gray('\n🔄 Force exit to prevent hanging...'));
      process.exit(0);
    }, 100);
    
  } catch (error) {
    console.error(chalk.red('\n💥 Fatal error:'), error);
    process.exit(1);
  }
}