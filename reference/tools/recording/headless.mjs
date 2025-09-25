// Headless AC - Shared functionality for bake.mjs and tape.mjs
// Provides common AC system initialization, API creation, and utilities

import { writeFileSync, readFileSync, existsSync } from 'fs';
import { resolve, dirname, join } from 'path';
import fs from 'fs';
import path from 'path';
import { pathToFileURL, fileURLToPath } from 'url';
import { PNG } from 'pngjs';
import { timestamp, resetRainbowCache, resetZebraCache, getRainbowState, setRainbowState, getZebraState, setZebraState } from "../../../system/public/aesthetic.computer/lib/num.mjs";
import chalk from 'chalk';
import ora from 'ora';
import { logInfo, logError, logWarning } from './logger.mjs';

// Simple PNG encoding function
function encodePNG(width, height, pixelBuffer) {
  const png = new PNG({ width, height });
  
  // Copy pixel buffer to PNG data
  for (let i = 0; i < pixelBuffer.length; i++) {
    png.data[i] = pixelBuffer[i];
  }
  
  return PNG.sync.write(png);
}

// Generate sixel format with full RGBA color support  
function pixelBufferToSixel(pixelBuffer, width, height, scale = 2) {
  let sixel = '\x1bPq'; // Start sixel mode
  
  const scaledWidth = width * scale;
  const scaledHeight = height * scale;
  
  const colors = new Map();
  let colorIndex = 0;
  
  // Pre-allocate band arrays to avoid repeated allocation
  const bandArrays = new Map();
  
  // Generate sixel data in 6-pixel high bands
  const bandsCount = Math.ceil(scaledHeight / 6);
  
  for (let band = 0; band < bandsCount; band++) {
    bandArrays.clear();
    
    for (let x = 0; x < scaledWidth; x++) {
      for (let dy = 0; dy < 6; dy++) {
        const scaledY = band * 6 + dy;
        if (scaledY >= scaledHeight) break;
        
        // Map back to original coordinates
        const origX = Math.floor(x / scale);
        const origY = Math.floor(scaledY / scale);
        
        if (origX < width && origY < height) {
          const i = (origY * width + origX) * 4;
          const r = pixelBuffer[i];
          const g = pixelBuffer[i + 1];
          const b = pixelBuffer[i + 2];
          const a = pixelBuffer[i + 3];
          
          // Skip transparent pixels
          if (a === 0) continue;
          
          // Simple color key to reduce string operations
          const colorKey = (r << 16) | (g << 8) | b;
          
          if (!colors.has(colorKey)) {
            colors.set(colorKey, colorIndex++);
            sixel += `#${colors.get(colorKey)};2;${Math.round(r*100/255)};${Math.round(g*100/255)};${Math.round(b*100/255)}`;
          }
          
          const color = colors.get(colorKey);
          if (!bandArrays.has(color)) {
            bandArrays.set(color, new Array(scaledWidth).fill(0));
          }
          bandArrays.get(color)[x] |= (1 << dy);
        }
      }
    }
    
    // Output band data
    for (const [color, pixels] of bandArrays) {
      sixel += `#${color}`;
      for (const pixel of pixels) {
        sixel += String.fromCharCode(63 + pixel);
      }
      sixel += '$';
    }
    
    // Move to next band
    if (band < bandsCount - 1) {
      sixel += '-';
    }
  }
  
  sixel += '\x1b\\'; // Exit sixel mode
  return sixel;
}

// Base headless AC environment
export class HeadlessAC {
  constructor(width = 128, height = 128, options = {}) {
    this.width = width;
    this.height = height;
    this.pixelBuffer = new Uint8Array(width * height * 4);
    this.graph = null;
    this.disk = null;
    this.text = null;
    this.typeface = null;
    this.typeModule = null;
    this.apiCalls = [];
    this.firstLineColorApplied = false; // Track if first-line color has been applied
    this.kidlispInstance = null; // Will be initialized when API is created
    this.kidlispState = null; // Will hold state to restore
    this.density = options.density || null; // Custom density parameter
    this.outputDir = options.outputDir || null; // Directory for saving embedded layer buffers
    
    // Performance tracking options
    this.detailedTiming = options.detailedTiming || false;
    this.operationTimings = new Map();
    
    // Embedded layer restoration
    this.deferredEmbeddedLayers = null; // Will hold embedded layers for restoration after KidLisp setup
    
    // Performance optimizations
    this.enableV8Optimizations();
    
    // Initialize with opaque black (like normal AC environment)
    // This ensures proper alpha blending behavior for semi-transparent elements
    for (let i = 0; i < this.pixelBuffer.length; i += 4) {
      this.pixelBuffer[i] = 0;     // R
      this.pixelBuffer[i + 1] = 0; // G
      this.pixelBuffer[i + 2] = 0; // B
      this.pixelBuffer[i + 3] = 255; // A (opaque)
    }
  }

  // Restore embedded layers after KidLisp API is set up
  restoreEmbeddedLayers() {
    if (!this.deferredEmbeddedLayers || !this.kidlispInstance || !this.KidLisp) {
      console.log(`🎬 HEADLESS DEBUG: Skipping embedded layer restoration - not ready yet`);
      return;
    }

    console.log(`🎬 HEADLESS DEBUG: Restoring ${this.deferredEmbeddedLayers.length} embedded layer definitions from previous frame`);
    
    // FIXED: Properly restore layers with their rendered content and KidLisp instances
    this.kidlispInstance.embeddedLayers = this.deferredEmbeddedLayers.map(layerMeta => {
      // Restore the pixel buffer from disk if available
      const buffer = layerMeta.buffer ? {
        width: layerMeta.buffer.width,
        height: layerMeta.buffer.height,
        pixels: layerMeta.buffer.filename ? 
          this.loadEmbeddedLayerBuffer(layerMeta.buffer.filename) : 
          new Uint8ClampedArray(layerMeta.buffer.width * layerMeta.buffer.height * 4) // Empty buffer
      } : null;
      
      // Create a minimal KidLisp instance for the embedded layer
      // This is required for updateEmbeddedLayer to work properly
      let kidlispInstance = null;
      console.log(`🔧 HEADLESS DEBUG: Attempting to restore KidLisp instance for ${layerMeta.cacheId}, hasMetaState=${!!layerMeta.kidlispInstanceState}, hasKidLispClass=${!!this.KidLisp}`);
      
      if (this.KidLisp) {
        // Create a new KidLisp instance for the embedded layer
        kidlispInstance = new this.KidLisp();
        
        // Restore minimal state for embedded layer instance
        if (layerMeta.kidlispInstanceState) {
          kidlispInstance.frameCount = layerMeta.kidlispInstanceState.frameCount || 0;
          kidlispInstance.localFrameCount = layerMeta.kidlispInstanceState.localFrameCount || 0;
        }
        
        // Set the source code for the layer
        if (layerMeta.sourceCode) {
          kidlispInstance.currentSource = layerMeta.sourceCode;
          // Parse the source code to create the parsedCode structure
          try {
            kidlispInstance.parsedCode = kidlispInstance.parse(layerMeta.sourceCode);
          } catch (parseError) {
            console.warn(`⚠️ Could not parse source code for layer ${layerMeta.cacheId}:`, parseError.message);
          }
        }
        
        console.log(`✅ HEADLESS DEBUG: Successfully restored KidLisp instance for layer: ${layerMeta.cacheId}`);
      } else {
        console.log(`❌ HEADLESS DEBUG: Could not restore KidLisp instance for ${layerMeta.cacheId} - missing requirements`);
      }
      
      return {
        ...layerMeta,
        buffer: buffer,
        kidlispInstance: kidlispInstance,
        // Restore parsed code if available
        parsedCode: kidlispInstance ? kidlispInstance.parsedCode : layerMeta.parsedCode
      };
    });

    // Populate cache with references to the restored layers
    // This is CRITICAL - without this, the system will recreate layers every frame
    if (this.kidlispInstance.embeddedLayers) {
      for (const layer of this.kidlispInstance.embeddedLayers) {
        if (layer.layerKey || layer.cacheId) {
          const cacheKey = layer.layerKey || layer.cacheId;
          this.kidlispInstance.embeddedLayerCache.set(cacheKey, layer);
          console.log(`🎯 HEADLESS DEBUG: Restored layer cache entry: ${cacheKey}`);
        }
      }
    }

    // Clear deferred layers
    this.deferredEmbeddedLayers = null;
  }

  // Save embedded layer buffer to disk (similar to background buffer)
  saveEmbeddedLayerBuffer(layerId, pixelData) {
    if (!this.outputDir) {
      console.warn('⚠️ No output directory set for saving embedded layer buffers');
      return null;
    }
    
    try {
      const filename = `embedded-layer-${layerId}.bin`;
      const filepath = path.join(this.outputDir, filename);
      fs.writeFileSync(filepath, pixelData);
      console.log(`💾 Saved embedded layer buffer: ${filename} (${pixelData.length} bytes)`);
      return filename;
    } catch (error) {
      console.warn(`⚠️ Failed to save embedded layer buffer for ${layerId}:`, error.message);
      return null;
    }
  }

  // Load embedded layer buffer from disk
  loadEmbeddedLayerBuffer(filename) {
    if (!this.outputDir || !filename) {
      return null;
    }
    
    try {
      const filepath = path.join(this.outputDir, filename);
      if (fs.existsSync(filepath)) {
        const bufferData = fs.readFileSync(filepath);
        console.log(`📥 Loaded embedded layer buffer: ${filename} (${bufferData.length} bytes)`);
        return new Uint8ClampedArray(bufferData);
      }
    } catch (error) {
      console.warn(`⚠️ Failed to load embedded layer buffer ${filename}:`, error.message);
    }
    return null;
  }

  // Set KidLisp state for restoration
  setKidlispState(state) {
    this.kidlispState = state;
    
    // If we have a KidLisp instance, restore the state immediately
    if (this.kidlispInstance && state) {
      // Basic state restoration
      if (state.onceExecuted) {
        this.kidlispInstance.onceExecuted = new Set(state.onceExecuted);
      }
      if (state.currentSource !== undefined) {
        this.kidlispInstance.currentSource = state.currentSource;
      }
      if (state.firstLineColor !== undefined) {
        this.kidlispInstance.firstLineColor = state.firstLineColor;
      }
      if (state.scrollFuzzDirection !== undefined) {
        this.kidlispInstance.scrollFuzzDirection = state.scrollFuzzDirection;
      }
      
      // Critical timing state restoration
      if (state.lastSecondExecutions) {
        this.kidlispInstance.lastSecondExecutions = { ...state.lastSecondExecutions };
      }
      if (state.sequenceCounters) {
        this.kidlispInstance.sequenceCounters = new Map(Object.entries(state.sequenceCounters));
      }
      if (state.frameCount !== undefined) {
        this.kidlispInstance.frameCount = state.frameCount;
      }
      if (state.instantTriggersExecuted) {
        this.kidlispInstance.instantTriggersExecuted = { ...state.instantTriggersExecuted };
      }
      
      // Advanced timing state restoration
      if (state.timingStates) {
        this.kidlispInstance.timingStates = new Map(Object.entries(state.timingStates));
      }
      if (state.activeTimingExpressions) {
        this.kidlispInstance.activeTimingExpressions = new Map(Object.entries(state.activeTimingExpressions));
      }
      
      // Store embedded layers for later restoration (after KidLisp class is available)
      if (state.embeddedLayers) {
        console.log(`🎬 HEADLESS DEBUG: Deferring restoration of ${state.embeddedLayers.length} embedded layer definitions (will restore after KidLisp API setup)`);
        this.deferredEmbeddedLayers = state.embeddedLayers;
      }
      
      // Initialize empty cache - will be populated by restoreEmbeddedLayers
      this.kidlispInstance.embeddedLayerCache = new Map();
      
      // Ink and visual state restoration
      if (state.inkState !== undefined) {
        this.kidlispInstance.inkState = state.inkState;
      }
      if (state.inkStateSet !== undefined) {
        this.kidlispInstance.inkStateSet = state.inkStateSet;
      }
      
      // Baked layers state restoration
      if (state.bakedLayers) {
        this.kidlispInstance.bakedLayers = [...state.bakedLayers];
      }
      if (state.bakeCallCount !== undefined) {
        this.kidlispInstance.bakeCallCount = state.bakeCallCount;
      }
      
      // Local environment restoration
      if (state.localEnv) {
        this.kidlispInstance.localEnv = { ...state.localEnv };
      }
      
      // Rainbow and zebra state restoration
      if (state.rainbowState) {
        setRainbowState(state.rainbowState);
      }
      if (state.zebraState) {
        setZebraState(state.zebraState);
      }
      
      console.log(`🔄 Restored KidLisp state: frame=${this.kidlispInstance.frameCount}, timing entries=${Object.keys(this.kidlispInstance.lastSecondExecutions).length}, sequence counters=${this.kidlispInstance.sequenceCounters.size}`);
    }
  }
  
  // Get current KidLisp state for saving
  getKidlispState() {
    if (this.kidlispInstance) {
      return {
        // Basic state (already captured)
        onceExecuted: Array.from(this.kidlispInstance.onceExecuted), // Convert Set to Array for JSON
        currentSource: this.kidlispInstance.currentSource,
        firstLineColor: this.kidlispInstance.firstLineColor,
        scrollFuzzDirection: this.kidlispInstance.scrollFuzzDirection,
        
        // Critical timing state for frame continuity
        lastSecondExecutions: this.kidlispInstance.lastSecondExecutions || {},
        sequenceCounters: this.kidlispInstance.sequenceCounters ? Object.fromEntries(this.kidlispInstance.sequenceCounters) : {},
        frameCount: this.kidlispInstance.frameCount || 0,
        instantTriggersExecuted: this.kidlispInstance.instantTriggersExecuted || {},
        
        // Advanced timing state
        timingStates: this.kidlispInstance.timingStates ? Object.fromEntries(this.kidlispInstance.timingStates) : {},
        activeTimingExpressions: this.kidlispInstance.activeTimingExpressions ? Object.fromEntries(this.kidlispInstance.activeTimingExpressions) : {},
        
        // Ink and visual state
        inkState: this.kidlispInstance.inkState,
        inkStateSet: this.kidlispInstance.inkStateSet || false,
        
        // Baked layers state
        bakedLayers: this.kidlispInstance.bakedLayers || [],
        bakeCallCount: this.kidlispInstance.bakeCallCount || 0,
        
        // Embedded layers state (CRITICAL for multi-frame rendering)
        // FIXED: Serialize layers WITH pixel buffers to maintain rendered content
        embeddedLayers: this.kidlispInstance.embeddedLayers ? this.kidlispInstance.embeddedLayers.map(layer => ({
          id: layer.id,
          x: layer.x,
          y: layer.y,
          width: layer.width,
          height: layer.height,
          alpha: layer.alpha,
          source: layer.source,
          sourceCode: layer.sourceCode,
          hasBeenEvaluated: layer.hasBeenEvaluated,
          lastFrameEvaluated: layer.lastFrameEvaluated,
          lastRenderTime: layer.lastRenderTime,
          cacheId: layer.cacheId,
          layerKey: layer.layerKey,
          localFrameCount: layer.localFrameCount || 0,
          timingPattern: layer.timingPattern,
          // Save buffer to disk and store metadata only
          buffer: layer.buffer ? {
            width: layer.buffer.width,
            height: layer.buffer.height,
            filename: this.saveEmbeddedLayerBuffer(layer.cacheId || layer.id || `layer_${Date.now()}`, layer.buffer.pixels)
          } : null,
          // Save minimal KidLisp instance state - mainly for frame counting
          kidlispInstanceState: layer.kidlispInstance ? {
            frameCount: layer.kidlispInstance.frameCount || 0,
            localFrameCount: layer.localFrameCount || 0
          } : null
        })) : [],
        // Save embedded layer cache structure with full layer references
        embeddedLayerCache: this.kidlispInstance.embeddedLayerCache ? Object.fromEntries(this.kidlispInstance.embeddedLayerCache) : {},
        
        // Local environment
        localEnv: this.kidlispInstance.localEnv || {},
        
        // Rainbow and zebra color cycling state
        rainbowState: getRainbowState(),
        zebraState: getZebraState()
      };
    }
    return {};
  }

  // Enable V8 optimizations for performance
  enableV8Optimizations() {
    // Hint to V8 that these are hot functions
    if (global.gc) {
      logInfo('💫 V8 optimizations: Garbage collection available');
    }
    
    // REDUCED ALLOCATIONS: Only allocate scratch buffers if really needed
    // this.scratchBuffer = new Uint8Array(this.width * this.height * 4);
    this.colorCache = new Map();
    
    logInfo(`🚀 Performance mode: Pixel buffer ${this.width}x${this.height} (${(this.pixelBuffer.length / 1024 / 1024).toFixed(2)}MB)`);
  }

  async initializeAC() {
    const spinner = ora(chalk.blue('🔧 Setting up full AC environment...')).start();
    
    try {
      // 🚀 Enable V8 performance optimizations
      if (typeof global.gc === 'function') {
        global.gc(); // Clean up before starting
        console.log('💫 Garbage collection available - optimizing memory');
      }
      
      // Optimize Node.js for graphics workloads
      if (process.env.NODE_ENV !== 'production') {
        process.env.NODE_ENV = 'production'; // Enable V8 optimizations
      }
      
      // Set up browser-like globals for Node.js
      global.window = global.window || global;
      global.document = global.document || {
        createElement: () => ({}),
        body: { appendChild: () => {} }
      };
      
      // Add requestAnimationFrame and other timing functions
      global.requestAnimationFrame = global.requestAnimationFrame || ((callback) => {
        return setTimeout(() => callback(Date.now()), 16);
      });
      global.cancelAnimationFrame = global.cancelAnimationFrame || clearTimeout;
      
      // Add performance API - deterministic timing support
      global.performance = global.performance || {
        now: () => global.ac?.api?.simulationTime || Date.now() // Use deterministic time if available
      };
      
      if (!global.navigator) {
        try {
          global.navigator = {
            userAgent: 'HeadlessAC/1.0',
            platform: 'HeadlessAC',
            language: 'en-US',
            onLine: true,
            cookieEnabled: false
          };
        } catch (e) {
          if (global.navigator) {
            Object.assign(global.navigator, {
              userAgent: 'HeadlessAC/1.0',
              platform: 'HeadlessAC'
            });
          }
        }
      }
      
      // Set up global.location with optional density parameter
      const searchParams = this.density ? `?density=${this.density}` : '';
      global.location = global.location || {
        href: `http://localhost:8888${searchParams}`,
        origin: 'http://localhost:8888',
        protocol: 'http:',
        host: 'localhost:8888',
        pathname: '/',
        search: searchParams,
        hash: ''
      };

      // Import and initialize graph.mjs
      spinner.text = 'Loading graph.mjs...';
      const graphModule = await import("../../../system/public/aesthetic.computer/lib/graph.mjs");
      this.graph = graphModule;
      
      // Create pixel buffer for graph.mjs
      const buffer = {
        width: this.width,
        height: this.height,
        pixels: this.pixelBuffer
      };
      
      this.graph.setBuffer(buffer);
      spinner.text = 'Pixel buffer initialized...';
      
      // 🧪 EXPERIMENTAL: Setup block processing if requested
      if (this.options && this.options.useBlockProcessing) {
        this.graph.setBlockProcessing(true);
        spinner.text = 'Block processing enabled...';
      }
      
      // Import the full disk.mjs system
      spinner.text = 'Loading disk.mjs...';
      const diskModule = await import("../../../system/public/aesthetic.computer/lib/disk.mjs");
      this.disk = diskModule;
      
      // Try to import text module for font debugging
      try {
        spinner.text = 'Loading text system...';
        const textModule = await import("../../../system/public/aesthetic.computer/lib/text.mjs");
        this.text = textModule;
        logInfo(chalk.green('✅ Text module loaded:'), Object.keys(textModule));
      } catch (error) {
        logWarning(chalk.yellow('⚠️ Text module not found:', error.message));
      }
      
      // Load the type system and create a typeface
      try {
        spinner.text = 'Loading font system...';
        const typeModule = await import("../../../system/public/aesthetic.computer/lib/type.mjs");
        this.typeModule = typeModule;
        
        // Create a typeface instance for font_1
        this.typeface = new typeModule.Typeface("font_1");
        
        // Override the load method to properly filter out non-string values
        const originalLoad = this.typeface.load.bind(this.typeface);
        this.typeface.load = async function($preload, needsPaintCallback) {
          if (this.name === "font_1") {
            // Filter entries to only include actual glyph paths (string values, not prefixed with "glyph")
            const glyphsToLoad = Object.entries(this.data).filter(
              ([g, loc]) => !g.startsWith("glyph") && typeof loc === 'string'
            );
            const promises = glyphsToLoad.map(([glyph, location], i) => {
              return $preload(
                `aesthetic.computer/disks/drawings/${this.name}/${location}.json`,
              )
                .then((res) => {
                  this.glyphs[glyph] = res;
                })
                .catch((err) => {
                  // Silently handle missing glyph files - some glyphs may not exist
                });
            });
            await Promise.all(promises);
          } else {
            // For other fonts, use the original method
            return originalLoad($preload, needsPaintCallback);
          }
        };
        
        // Create a mock $preload function for loading glyph data
        const mockPreload = async (path) => {
          try {
            // Convert AC path to actual file path using absolute path resolution
            const __filename = fileURLToPath(import.meta.url);
            const __dirname = dirname(__filename);
            const actualPath = resolve(__dirname, '../../../system/public', path);
            
            // Read JSON file directly
            const jsonData = readFileSync(actualPath, 'utf8');
            return JSON.parse(jsonData);
          } catch (error) {
            console.warn(`⚠️ Could not load glyph: ${path}`, error.message);
            return null;
          }
        };
        
        // Load the font glyphs
        await this.typeface.load(mockPreload);
        logInfo(chalk.green('✅ Font system loaded with glyphs:'), Object.keys(this.typeface.glyphs).length, 'characters');
        
        // Debug first few glyphs to see what we loaded
        const glyphKeys = Object.keys(this.typeface.glyphs).slice(0, 5);
        for (const key of glyphKeys) {
          const glyph = this.typeface.glyphs[key];
          if (glyph && Array.isArray(glyph)) {
            console.log(`🔍 Glyph "${key}" (char ${key.charCodeAt(0)}): ${glyph.length} commands`);
            if (glyph.length > 0) {
              console.log(`📝 First command:`, glyph[0]);
            }
          }
        }
        
      } catch (error) {
        console.log(chalk.yellow('⚠️ Font system load failed:', error.message));
        this.typeface = null;
      }
      
      spinner.succeed(chalk.green('🚀 AC system loaded successfully'));
      
    } catch (error) {
      spinner.fail(chalk.red('💥 Error loading AC system'));
      console.error(chalk.red(error));
      throw error;
    }
  }
  
  async createAPI() {
    const self = this;
    
    function logCall(name, args) {
      // DISABLED: Skip API call logging during video recording to reduce memory pressure
      // self.apiCalls.push({ name, args: Array.from(args), timestamp: Date.now() });
    }

    // Performance timing wrapper for graph operations
    function timeGraphOperation(operationName, graphFunc, ...args) {
      const result = graphFunc(...args);
      
      // DISABLED: Skip timing logs during video recording to save memory
      // Only track for critical operations or debugging
      if (self.detailedTiming && operationName === 'critical_debug_only') {
        const startTime = process.hrtime.bigint();
        const endTime = process.hrtime.bigint();
        const duration = Number(endTime - startTime) / 1000000;
        console.log(`⚡ ${operationName}: ${duration.toFixed(3)}ms`);
      }
      
      return result;
    }

    // Try to use real disk.mjs API if available
    if (this.disk && this.graph) {
      logInfo('🔧 Creating graph-based API...');
      
      const api = {};
      
      // Basic properties
      api.screen = { 
        width: this.width, 
        height: this.height,
        pixels: this.pixelBuffer 
      };
      api.pen = { x: 0, y: 0 };
      
      // Drawing functions that call graph.mjs directly
      api.wipe = function(...args) {
        logCall('wipe', args);
        if (args.length > 0) {
          const foundColor = timeGraphOperation('findColor', self.graph.findColor.bind(self.graph), ...args);
          timeGraphOperation('color', self.graph.color.bind(self.graph), ...foundColor);
        }
        // Only clear if we haven't restored a background buffer
        // This prevents wiping out the accumulated frame buffer during multi-frame rendering
        if (!self.backgroundBufferRestored) {
          timeGraphOperation('clear', self.graph.clear.bind(self.graph));
        } else {
          console.log('🎨 Skipping clear - background buffer was restored');
        }
        return api;
      };
      
      api.ink = function(...args) {
        logCall('ink', args);
        const foundColor = timeGraphOperation('findColor', self.graph.findColor.bind(self.graph), ...args);
        timeGraphOperation('color', self.graph.color.bind(self.graph), ...foundColor);
        return api;
      };
      
      api.line = function(...args) {
        logCall('line', args);
        timeGraphOperation('line', self.graph.line.bind(self.graph), ...args);
        return api;
      };
      
      api.circle = function(...args) {
        logCall('circle', args);
        timeGraphOperation('circle', self.graph.circle.bind(self.graph), ...args);
        return api;
      };
      
      api.write = function(...args) {
        logCall('write', args);
        
        // Use the real AC typeface system if available
        if (self.typeface && Object.keys(self.typeface.glyphs).length > 0) {
          try {
            // Create a mock $ object with the functions typeface.print needs
            const mockAPI = {
              screen: { width: self.width, height: self.height },
              inkrn: () => {
                // Return the actual current color from graph system
                if (self.graph && self.graph.c) {
                  return self.graph.c.slice(); // Return copy of current color
                }
                return [255, 255, 255]; // Fallback to white
              },
              ink: (...color) => {
                if (color.length > 0) {
                  const foundColor = self.graph.findColor(...color);
                  self.graph.color(...foundColor);
                }
                return mockAPI;
              },
              box: (x, y, w, h) => {
                self.graph.box(x, y, w, h);
                return mockAPI;
              },
              printLine: (text, font, x, y, blockWidth, size, xOffset, thickness, rotation, fontData) => {
                // Use graph.mjs printLine function directly
                if (self.graph && self.graph.printLine) {
                  self.graph.printLine(text, font, x, y, blockWidth, size, xOffset, thickness, rotation, fontData);
                } else {
                  console.warn('⚠️ graph.printLine not available');
                }
                return mockAPI;
              },
              num: {
                randIntRange: (min, max) => Math.floor(Math.random() * (max - min + 1)) + min
              }
            };
            
            // Parse arguments like AC's write function
            let x, y, text, size = 0, pos = {};
            if (args.length >= 4) {
              [text, x, y, size] = args;
              pos = { x, y };
            } else if (args.length >= 3) {
              [text, x, y] = args;
              pos = { x, y };
            } else if (args.length === 2) {
              [text, pos] = args;
            } else if (args.length === 1) {
              text = args[0];
            }
            
            // Call the real typeface print method with proper size
            self.typeface.print(mockAPI, pos, size, text);
            
          } catch (error) {
            console.warn('⚠️ Typeface rendering failed, using fallback:', error.message);
            // Fallback to simple text rendering
            if (args.length >= 3) {
              self.renderSimpleText(args[0], args[1], args[2]);
            }
          }
        } else {
          // Fallback to simple text rendering if no typeface available
          if (args.length >= 3) {
            self.renderSimpleText(args[0], args[1], args[2]);
          }
        }
        
        return api;
      };
      
      api.rect = function(...args) {
        logCall('rect', args);
        timeGraphOperation('box', self.graph.box.bind(self.graph), ...args);
        return api;
      };
      
      api.box = function(...args) {
        logCall('box', args);
        timeGraphOperation('box', self.graph.box.bind(self.graph), ...args);
        return api;
      };
      
      api.point = function(...args) {
        logCall('point', args);
        timeGraphOperation('point', self.graph.point.bind(self.graph), ...args);
        return api;
      };
      
      api.plot = function(...args) {
        logCall('plot', args);
        timeGraphOperation('point', self.graph.point.bind(self.graph), ...args);
        return api;
      };
      
      // Add transformation functions that work with graph.mjs system
      api.spin = function(...args) {
        logCall('spin', args);
        console.log(`🔄 Spin: ${args.join(', ')}`);
        
        // Call the real graph.mjs spin function
        if (self.graph && self.graph.spin) {
          timeGraphOperation('spin', self.graph.spin.bind(self.graph), ...args);
        } else {
          console.log('⚠️ Graph spin not available, using fallback');
          if (args.length > 0) {
            self.applySpinTransformation(args[0]);
          }
        }
        return api;
      };
      
      api.zoom = function(...args) {
        logCall('zoom', args);
        console.log(`🔍 Zoom: ${args.join(', ')}`);
        
        // Call the real graph.mjs zoom function  
        if (self.graph && self.graph.zoom) {
          timeGraphOperation('zoom', self.graph.zoom.bind(self.graph), ...args);
        } else {
          console.log('⚠️ Graph zoom not available, using fallback');
          if (args.length > 0) {
            self.applyZoomTransformation(args[0]);
          }
        }
        return api;
      };
      
      api.contrast = function(...args) {
        logCall('contrast', args);
        console.log(`🎨 Contrast: ${args.join(', ')}`);
        
        // Call the real graph.mjs contrast function
        if (self.graph && self.graph.contrast) {
          timeGraphOperation('contrast', self.graph.contrast.bind(self.graph), ...args);
        } else {
          console.log('⚠️ Graph contrast not available, using fallback');
          if (args.length > 0) {
            self.applyContrastTransformation(args[0]);
          }
        }
        return api;
      };
      
      api.blur = function(...args) {
        logCall('blur', args);
        const logMessage = `Blur: ${args.join(', ')}`;
        if (self.logger && typeof self.logger === 'function') {
          self.logger('blur', logMessage);
        } else {
          console.log(`🌀 ${logMessage}`);
        }
        
        // Call the real graph.mjs blur function
        if (self.graph && self.graph.blur) {
          self.graph.blur(...args);
        } else {
          console.log('⚠️ Graph blur not available, using fallback');
          if (args.length > 0) {
            self.applyBlurTransformation(args[0]);
          }
        }
        return api;
      };
      
      api.scroll = function(...args) {
        logCall('scroll', args);
        logInfo(`📜 Scroll: ${args.join(', ')}`);
        
        // Call the real graph.mjs scroll function
        if (self.graph && self.graph.scroll) {
          timeGraphOperation('scroll', self.graph.scroll.bind(self.graph), ...args);
        } else {
          console.log('⚠️ Graph scroll not available, using fallback');
          if (args.length >= 2) {
            self.applyScrollTransformation(args[0], args[1]);
          }
        }
        return api;
      };
      
      // Add comprehensive num utilities needed by AC disks
      api.num = {
        radians: (degrees) => degrees * (Math.PI / 180),
        degrees: (radians) => radians * (180 / Math.PI),
        randInt: (max) => Math.floor(Math.random() * max),
        randIntRange: (min, max) => Math.floor(Math.random() * (max - min + 1)) + min,
        map: (value, start1, stop1, start2, stop2) => {
          return start2 + (stop2 - start2) * ((value - start1) / (stop1 - start1));
        },
        lerp: (start, stop, amount) => start + (stop - start) * amount,
        clamp: (value, min, max) => Math.max(min, Math.min(max, value)),
        dist: (x1, y1, x2, y2) => Math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2),
        timestamp: () => api.simulationTime || Date.now() // Use deterministic time if available
      };
      
      // Add clock utilities for timing-based animations
      api.clock = {
        time: () => api.simulationTime ? new Date(api.simulationTime) : new Date()
      };
      
      // Initialize simulation time tracking
      api.simulationTime = null; // Will be set by tape.mjs for deterministic recording
      
      // Set up global access for deterministic timing
      global.ac = global.ac || {};
      global.ac.api = api;
      
      // Add KidLisp-specific functions that are missing
      
      // Flood function - fills connected areas with current ink color
      api.flood = function(...args) {
        logCall('flood', args);
        console.log(`🌊 Flood: ${args.join(', ')}`);
        
        // Basic flood fill implementation
        if (self.graph && self.graph.flood) {
          console.log(`🔧 FLOOD DEBUG: Using graph.flood, api.screen sample before:`, Array.from(api.screen.pixels.slice(0, 20)));
          self.graph.flood(...args);
          console.log(`🔧 FLOOD DEBUG: Using graph.flood, api.screen sample after:`, Array.from(api.screen.pixels.slice(0, 20)));
        } else {
          console.log('⚠️ Graph flood not available, using simple fill');
          // Simple fill the entire screen as fallback
          const [r, g, b, a] = self.currentColor || [255, 255, 255, 255];
          for (let i = 0; i < self.pixelBuffer.length; i += 4) {
            self.pixelBuffer[i] = r;
            self.pixelBuffer[i + 1] = g;
            self.pixelBuffer[i + 2] = b;
            self.pixelBuffer[i + 3] = a;
          }
        }
        return api;
      };
      
      // Color name functions - KidLisp uses these as background setters
      api.black = function() {
        logCall('black', []);
        console.log('🖤 Setting background: black');
        self.graph.color(0, 0, 0, 255);
        self.graph.clear();
        return api;
      };
      
      api.salmon = function() {
        logCall('salmon', []);
        console.log('🍣 Setting background: salmon');
        self.graph.color(250, 128, 114, 255);
        self.graph.clear();
        return api;
      };
      
      api.white = function() {
        logCall('white', []);
        console.log('🤍 Setting background: white');
        self.graph.color(255, 255, 255, 255);
        self.graph.clear();
        return api;
      };
      
      api.gray = function() {
        logCall('gray', []);
        console.log('🩶 Setting background: gray');
        self.graph.color(128, 128, 128, 255);
        self.graph.clear();
        return api;
      };
      
      // Random choice function - KidLisp uses (?) for random values
      api['?'] = function(...choices) {
        logCall('?', choices);
        if (choices.length === 0) return 0;
        const randomIndex = Math.floor(Math.random() * choices.length);
        const result = choices[randomIndex];
        console.log(`🎲 Random choice from [${choices.join(', ')}]: ${result}`);
        return result;
      };
      
      // Width and height variables that KidLisp pieces often use
      api.w = self.width;
      api.h = self.height;
      api.width = self.width;
      api.height = self.height;
      
      // Page function for switching drawing buffers (needed by KidLisp embed system)
      api.page = function(buffer) {
        if (buffer && buffer.width && buffer.height && buffer.pixels) {
          // Switch to the new buffer
          api.screen = {
            width: buffer.width,
            height: buffer.height,
            pixels: buffer.pixels
          };
          // Update the graph drawing context
          if (self.graph && self.graph.setBuffer) {
            self.graph.setBuffer(buffer);
            console.log(`🔧 PAGE DEBUG: Graph setBuffer called, buffer sample:`, Array.from(buffer.pixels.slice(0, 20)));
          }
          console.log(`📄 Switched to buffer: ${buffer.width}x${buffer.height}`);
          console.log(`🔍 PAGE DEBUG: api.screen.pixels sample:`, Array.from(api.screen.pixels.slice(0, 20)));
        } else {
          console.warn('⚠️ page() called with invalid buffer:', buffer);
        }
      };
      
      // Write function for text rendering (used by embedded KidLisp pieces)
      api.write = function(...args) {
        logCall('write', args);
        const logMessage = `Write: ${args[0] || ''}`;
        if (self.logger && typeof self.logger === 'function') {
          self.logger('write', logMessage);
        } else {
          // console.log(`📝 ${logMessage}`); // Commented out for performance during recording
        }
        
        // Use the real AC typeface system if available
        if (self.typeface && Object.keys(self.typeface.glyphs).length > 0) {
          try {
            // Create a mock $ object with the functions typeface.print needs
            const mockAPI = {
              screen: { width: self.width, height: self.height },
              inkrn: () => {
                // Return the actual current color from graph system
                if (self.graph && self.graph.c) {
                  return self.graph.c.slice(); // Return copy of current color
                }
                return [255, 255, 255]; // Fallback to white
              },
              ink: (...color) => {
                if (color.length > 0) {
                  const foundColor = self.graph.findColor(...color);
                  self.graph.color(...foundColor);
                }
                return mockAPI;
              },
              box: (x, y, w, h) => {
                self.graph.box(x, y, w, h);
                return mockAPI;
              },
              printLine: (text, font, x, y, blockWidth, size, xOffset, thickness, rotation, fontData) => {
                // Use graph.mjs printLine function directly
                if (self.graph && self.graph.printLine) {
                  self.graph.printLine(text, font, x, y, blockWidth, size, xOffset, thickness, rotation, fontData);
                } else {
                  console.warn('⚠️ graph.printLine not available');
                }
                return mockAPI;
              },
              num: {
                randIntRange: (min, max) => Math.floor(Math.random() * (max - min + 1)) + min
              }
            };
            
            // Parse arguments like AC's write function
            let x, y, text, size = 0, pos = {};
            if (args.length >= 4) {
              [text, x, y, size] = args;
              pos = { x, y };
            } else if (args.length >= 3) {
              [text, x, y] = args;
              pos = { x, y };
            } else if (args.length === 2) {
              [text, pos] = args;
            } else if (args.length === 1) {
              text = args[0];
            }
            
            // Call the real typeface print method with proper size
            self.typeface.print(mockAPI, pos, size, text);
            
          } catch (error) {
            console.warn('⚠️ Typeface rendering failed, using fallback:', error.message);
          }
        } else {
          console.warn('⚠️ No typeface system available for text rendering');
        }
      };
      
      // Paste function for compositing embedded buffers back to main canvas
      api.paste = function(sourceBuffer, x = 0, y = 0) {
        logCall('paste', [sourceBuffer ? `${sourceBuffer.width}x${sourceBuffer.height}` : 'invalid', x, y]);
        if (sourceBuffer && sourceBuffer.pixels && api.screen && api.screen.pixels) {
          console.log(`🎨 Paste: Compositing ${sourceBuffer.width}x${sourceBuffer.height} buffer at (${x},${y})`);
          // For headless, we'd need to implement actual pixel compositing here
          // For now, just log that we're doing the paste operation
          console.log(`🎨 Paste: First pixel of source:`, sourceBuffer.pixels[0], sourceBuffer.pixels[1], sourceBuffer.pixels[2], sourceBuffer.pixels[3]);
        } else {
          console.warn('⚠️ Paste called with invalid buffers');
        }
      };
      
      // Paste function for compositing embedded buffers (critical for KidLisp embed system)
      api.paste = function(sourceBuffer, x = 0, y = 0) {
        logCall('paste', [`${sourceBuffer?.width}x${sourceBuffer?.height}`, x, y]);
        
        if (!sourceBuffer || !sourceBuffer.pixels || !api.screen || !api.screen.pixels) {
          console.warn('⚠️ paste() called with invalid buffers');
          return;
        }
        
        console.log(`🎨 Pasting ${sourceBuffer.width}x${sourceBuffer.height} buffer to (${x}, ${y})`);
        
        // Simple pixel-level compositing - copy sourceBuffer to api.screen at position (x, y)
        const srcWidth = sourceBuffer.width;
        const srcHeight = sourceBuffer.height;
        const dstWidth = api.screen.width;
        const dstHeight = api.screen.height;
        
        for (let sy = 0; sy < srcHeight; sy++) {
          for (let sx = 0; sx < srcWidth; sx++) {
            const dx = x + sx;
            const dy = y + sy;
            
            // Skip pixels outside destination bounds
            if (dx < 0 || dy < 0 || dx >= dstWidth || dy >= dstHeight) continue;
            
            const srcIndex = (sy * srcWidth + sx) * 4;
            const dstIndex = (dy * dstWidth + dx) * 4;
            
            // Copy RGBA values
            api.screen.pixels[dstIndex] = sourceBuffer.pixels[srcIndex];         // R
            api.screen.pixels[dstIndex + 1] = sourceBuffer.pixels[srcIndex + 1]; // G
            api.screen.pixels[dstIndex + 2] = sourceBuffer.pixels[srcIndex + 2]; // B
            api.screen.pixels[dstIndex + 3] = sourceBuffer.pixels[srcIndex + 3]; // A
          }
        }
        
        console.log(`✅ Paste completed: ${srcWidth}x${srcHeight} → (${x}, ${y})`);
      };
      
      // Note: embed function should be provided by KidLisp's global environment
      // via getGlobalEnv(), not the API. If $code evaluation isn't working,
      // the issue is likely that the global environment isn't properly set up.
      
      // Add text API that references the actual text module functions
      // This mirrors how $commonApi.text is structured in disk.mjs but uses the loaded modules
      api.text = {
        // Use actual text module functions
        capitalize: self.text ? self.text.capitalize : (str) => str.charAt(0).toUpperCase() + str.slice(1),
        reverse: self.text ? self.text.reverse : (str) => str.split('').reverse().join(''),
        
        // These mirror the disk.mjs $commonApi.text implementation
        width: (text) => {
          if (Array.isArray(text)) text = text.join(" ");
          return text.length * 6; // blockWidth = 6
        },
        
        height: (text) => {
          return 10; // blockHeight = 10
        },
        
        // Use the actual box function logic from disk.mjs but adapted for headless
        box: function(text, pos = { x: 0, y: 0 }, bounds, scale = 1, wordWrap = true) {
          // Try to call the actual implementation from disk.mjs if available
          if (self.disk && self.disk.$commonApi && self.disk.$commonApi.text && self.disk.$commonApi.text.box) {
            console.log(`🎯 Using real disk.mjs text.box for "${text}" scale=${scale}`);
            return self.disk.$commonApi.text.box(text, pos, bounds, scale, wordWrap);
          }
          
          // Fallback to simplified version
          console.log(`⚠️ Using fallback text.box for "${text}" scale=${scale}`);
          if (!text) {
            console.warn("⚠️ No text for `box`.");
            return;
          }
          
          // Use the same logic as disk.mjs $commonApi.text.box
          pos = { ...pos };
          let run = 0;
          
          // Use actual scale without adjustment to match text rendering
          const adjustedScale = scale; // Use the same scale as text rendering
          const tf = self.typeface || { blockWidth: 6, blockHeight: 10 };
          const blockWidth = tf.blockWidth * Math.abs(adjustedScale);

          const lines = [[]];
          let line = 0;

          if (bounds === undefined) bounds = (text.length + 2) * blockWidth;

          function newLine() {
            run = 0;
            line += 1;
            lines[line] = [];
          }

          // Simplified word wrapping logic matching disk.mjs
          if (wordWrap) {
            const words = text.split(" ");
            words.forEach((word, wordIndex) => {
              const wordLen = word.length * blockWidth;
              const spaceWidth = blockWidth;
              const spaceNeeded = run > 0 ? spaceWidth : 0;
              
              if (run + spaceNeeded + wordLen >= bounds) newLine();
              lines[line].push(word);
              run += wordLen + (wordIndex < words.length - 1 ? spaceWidth : 0);
            });
          } else {
            lines[0] = [text];
          }

          const blockHeight = tf.blockHeight * adjustedScale; // Full line height including spacing
          
          // For single-line text, use just the character height without extra line spacing
          let height;
          if (lines.length === 1) {
            // Single line: use the actual blockHeight (which should be the glyph height)
            height = blockHeight;
          } else {
            // Multiple lines: add a small gap between lines (like +1px per line for spacing)
            const lineSpacing = adjustedScale; // 1px scaled up
            height = lines.length * blockHeight + (lines.length - 1) * lineSpacing;
          }
          
          let maxLineWidth = 0;
          lines.forEach(line => {
            if (line.length > 0) {
              const lineText = line.join(' ');
              const lineWidth = lineText.length * blockWidth;
              maxLineWidth = Math.max(maxLineWidth, lineWidth);
            }
          });
          
          const box = { x: pos.x, y: pos.y, width: maxLineWidth, height };
          return { pos, box, lines };
        }
      };
      
      console.log('✅ Text API exposed using actual text module functions: capitalize, reverse, width, height, box');
      
      // Add KidLisp integration by creating a direct instance
      try {
        // Import KidLisp directly from the kidlisp module
        const { KidLisp } = await import("../../../system/public/aesthetic.computer/lib/kidlisp.mjs");
        
        // Store KidLisp class for use in embedded layer restoration
        self.KidLisp = KidLisp;
        
        if (!self.kidlispInstance) {
          self.kidlispInstance = new KidLisp();
          
          // Override Date.now() for the KidLisp instance to use simulation time
          const originalDateNow = Date.now;
          self.kidlispInstance.getSimulationTime = () => {
            return self.simulationTime || originalDateNow();
          };
          
          // Patch the KidLisp instance to use simulation time
          self.kidlispInstance.originalDateNow = originalDateNow;
          
          // Reset timing state since we're switching to simulation time
          self.kidlispInstance.lastSecondExecutions = {};
          self.kidlispInstance.sequenceCounters = new Map();
          console.log('🔄 Reset KidLisp timing state for simulation mode');
          
          // Restore KidLisp state if available - use the enhanced state restoration
          if (self.kidlispState) {
            console.log('🔄 Restoring comprehensive KidLisp state from previous frame');
            
            // Use the setKidlispState method which handles all state restoration
            self.setKidlispState(self.kidlispState);
            
            console.log('✅ Complete KidLisp state restored');
          }
          
          // Note: setAPI will be called in the kidlisp function with the current API
        }
        
        api.kidlisp = function(x = 0, y = 0, width, height, source, options = {}) {
          logCall('kidlisp', [x, y, width, height, source, options]);
          
          console.log('DEBUG: API object keys:', Object.keys(api));
          console.log('DEBUG: API screen:', api.screen);
          console.log('DEBUG: Self width/height:', self.width, self.height);
          
          // Set default width/height to screen dimensions if not provided
          if (width === undefined) width = self.width;
          if (height === undefined) height = self.height;
          
          // Update the KidLisp instance with the current API before evaluation
          self.kidlispInstance.setAPI(api);
          
          // Now that KidLisp API is set up, restore any deferred embedded layers
          self.restoreEmbeddedLayers();
          
          // Simulate KidLisp sim function behavior to properly advance frame count and rainbow cache
          // This MUST happen before any KidLisp evaluation to ensure proper color cycling
          self.kidlispInstance.frameCount++; // Increment frame counter for timing functions
          resetRainbowCache(); // Reset rainbow cache for new frame to ensure color cycling
          
          // Set KidLisp context in graph system for dynamic fade evaluation
          if (self.graph && self.graph.setKidLispContext) {
            self.graph.setKidLispContext(self.kidlispInstance, api, self.kidlispInstance.localEnv);
          }
          
          // Execute the KidLisp code with proper first-line color detection
          try {
            // Parse the source code first
            console.log(`DEBUG: KidLisp source before parsing:`, JSON.stringify(source));
            self.kidlispInstance.parse(source);
            console.log(`DEBUG: KidLisp AST after parsing:`, self.kidlispInstance.ast);
            
            if (self.kidlispInstance.ast) {
              // Detect and apply first-line color if needed (only on frame 0, like "once wipe purple")
              self.kidlispInstance.detectFirstLineColor();
              if (self.kidlispInstance.firstLineColor && (api.frameIndex === undefined || api.frameIndex === 0)) {
                console.log(`🎨 Detected first-line color: ${self.kidlispInstance.firstLineColor} (applying on frame 0 only)`);
                api.wipe(self.kidlispInstance.firstLineColor);
              } else if (self.kidlispInstance.firstLineColor) {
                console.log(`🎨 First-line color ${self.kidlispInstance.firstLineColor} detected but skipping wipe (frame ${api.frameIndex})`);
              }
              
              // Evaluate using the main KidLisp evaluation system
              
              // Pass the AST directly - if it's already an array, use it as the body
              const astToEvaluate = self.kidlispInstance.ast.body || self.kidlispInstance.ast;
              
              const result = self.kidlispInstance.evaluate(
                astToEvaluate, 
                api, 
                self.kidlispInstance.localEnv
              );
              
              console.log(`🎯 KidLisp evaluation result:`, result);
              return result;
            }
          } catch (error) {
            console.error('KidLisp execution error:', error);
            return null;
          }
        };
        
        console.log('✅ KidLisp API exposed using direct KidLisp instance');
      } catch (error) {
        console.warn('⚠️ Failed to load KidLisp module:', error.message);
      }
      
      // Add direct references for destructuring patterns used by some disks
      api.api = api; // Self reference for when destructured as { api }
      
      return api;
    }
    
    // Fallback basic API
    console.log('🔄 Using basic API fallback');
    return this.createBasicAPI(logCall);
  }
  
  createBasicAPI(logCall) {
    return {
      screen: { width: this.width, height: this.height },
      pen: { x: 0, y: 0 },
      
      wipe: (...args) => {
        logCall('wipe', args);
        // Simple clear to black
        for (let i = 0; i < this.pixelBuffer.length; i += 4) {
          this.pixelBuffer[i] = 0;
          this.pixelBuffer[i + 1] = 0;
          this.pixelBuffer[i + 2] = 0;
          this.pixelBuffer[i + 3] = 255;
        }
        return this;
      },
      
      ink: (...args) => {
        logCall('ink', args);
        return this;
      },
      
      write: (...args) => {
        logCall('write', args);
        return this;
      },
      
      // Page function for basic API (needed by KidLisp embed system)
      page: (buffer) => {
        logCall('page', [buffer ? `${buffer.width}x${buffer.height}` : 'invalid']);
        if (buffer && buffer.width && buffer.height && buffer.pixels) {
          console.log(`📄 Basic API: Switched to buffer: ${buffer.width}x${buffer.height}`);
          // For basic API, we just acknowledge the page switch but don't change much
          // The real work happens in the KidLisp embed system
        } else {
          console.warn('⚠️ Basic API: page() called with invalid buffer:', buffer);
        }
        return this;
      },
      
      // Paste function for compositing embedded buffers  
      paste: (sourceBuffer, x = 0, y = 0) => {
        logCall('paste', [sourceBuffer ? `${sourceBuffer.width}x${sourceBuffer.height}` : 'invalid', x, y]);
        if (sourceBuffer && sourceBuffer.pixels) {
          console.log(`🎨 Basic API: Paste ${sourceBuffer.width}x${sourceBuffer.height} at (${x},${y})`);
        } else {
          console.warn('⚠️ Basic API: paste() called with invalid buffer');
        }
        return this;
      },
      
      // Paste function for basic API (needed by KidLisp embed system)
      paste: (sourceBuffer, x = 0, y = 0) => {
        logCall('paste', [`${sourceBuffer?.width}x${sourceBuffer?.height}`, x, y]);
        
        if (!sourceBuffer || !sourceBuffer.pixels) {
          console.warn('⚠️ Basic API: paste() called with invalid sourceBuffer');
          return this;
        }
        
        console.log(`🎨 Basic API: Pasting ${sourceBuffer.width}x${sourceBuffer.height} buffer to (${x}, ${y})`);
        
        // For the basic API, we can't do much actual compositing since we don't have sophisticated buffer management
        // But we log it so we know the paste is being called
        console.log(`✅ Basic API: Paste acknowledged (no actual compositing in basic mode)`);
        return this;
      },
      
      // Add transformation functions for basic API as well
      spin: (...args) => {
        logCall('spin', args);
        console.log(`🔄 Spin: ${args.join(', ')}`);
        return this;
      },
      
      zoom: (...args) => {
        logCall('zoom', args);
        console.log(`🔍 Zoom: ${args.join(', ')}`);
        return this;
      },
      
      contrast: (...args) => {
        logCall('contrast', args);
        console.log(`🎨 Contrast: ${args.join(', ')}`);
        return this;
      },
      
      scroll: (...args) => {
        logCall('scroll', args);
        console.log(`📜 Scroll: ${args.join(', ')}`);
        return this;
      },
      
      // Add clock utilities for timing-based animations
      clock: {
        time: () => {
          const simulationTime = this.api?.simulationTime || Date.now();
          return new Date(simulationTime);
        }
      },
      
      // Add num utilities
      num: {
        radians: (degrees) => degrees * (Math.PI / 180),
        degrees: (radians) => radians * (180 / Math.PI),
        randInt: (max) => Math.floor(Math.random() * max),
        randIntRange: (min, max) => Math.floor(Math.random() * (max - min + 1)) + min,
        map: (value, start1, stop1, start2, stop2) => {
          return start2 + (stop2 - start2) * ((value - start1) / (stop1 - start1));
        },
        lerp: (start, stop, amount) => start + (stop - start) * amount,
        clamp: (value, min, max) => Math.max(min, Math.min(max, value)),
        dist: (x1, y1, x2, y2) => Math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2),
        timestamp: Date.now()
      },
      
      // Add basic text API fallback
      text: {
        box: (text, pos = { x: 0, y: 0 }, bounds, scale = 1, wordWrap = true) => {
          // Basic text box calculation
          const blockWidth = 6 * Math.abs(scale);
          const blockHeight = 10 * Math.abs(scale);
          
          if (!text) {
            console.warn("⚠️ No text for basic `text.box`.");
            return { pos, box: { x: pos.x, y: pos.y, width: 0, height: 0 }, lines: [] };
          }
          
          // Simple line breaking for basic mode
          const lines = text.toString().split('\\n');
          let maxWidth = 0;
          
          lines.forEach(line => {
            maxWidth = Math.max(maxWidth, line.length * blockWidth);
          });
          
          const height = lines.length * blockHeight;
          
          return {
            pos,
            box: { x: pos.x, y: pos.y, width: maxWidth, height },
            lines: lines.map(line => [line])
          };
        },
        width: (text) => text ? text.toString().length * 6 : 0,
        height: (text) => 10
      }
    };
  }
  
  // Simple text rendering fallback
  renderSimpleText(text, x, y) {
    const str = text.toString();
    const charWidth = 6;
    const charHeight = 10;
    
    for (let i = 0; i < str.length; i++) {
      const charX = x + (i * charWidth);
      
      // Draw simple 6x10 character outline
      for (let dy = 0; dy < charHeight; dy++) {
        for (let dx = 0; dx < charWidth; dx++) {
          // Simple character shape
          const shouldDraw = (dy === 0 || dy === charHeight - 1) ||
                           (dx === 0) || (dx === charWidth - 3);
          
          if (shouldDraw && this.graph && this.graph.point) {
            this.graph.point(charX + dx, y + dy);
          }
        }
      }
    }
  }

  // Pixel-level transformation methods
  applySpinTransformation(steps) {
    if (!this.pixelBuffer) return;
    
    const buffer = new Uint8Array(this.pixelBuffer);
    const centerX = this.width / 2;
    const centerY = this.height / 2;
    const angle = (steps || 0) * 0.01; // Convert steps to radians
    
    // Simple rotation transformation
    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        const dx = x - centerX;
        const dy = y - centerY;
        
        // Rotate point
        const rotX = Math.cos(angle) * dx - Math.sin(angle) * dy + centerX;
        const rotY = Math.sin(angle) * dx + Math.cos(angle) * dy + centerY;
        
        // Check bounds and copy pixel
        if (rotX >= 0 && rotX < this.width && rotY >= 0 && rotY < this.height) {
          const srcIdx = (Math.floor(rotY) * this.width + Math.floor(rotX)) * 4;
          const dstIdx = (y * this.width + x) * 4;
          
          if (srcIdx >= 0 && srcIdx < buffer.length - 3) {
            this.pixelBuffer[dstIdx] = buffer[srcIdx];
            this.pixelBuffer[dstIdx + 1] = buffer[srcIdx + 1];
            this.pixelBuffer[dstIdx + 2] = buffer[srcIdx + 2];
            this.pixelBuffer[dstIdx + 3] = buffer[srcIdx + 3];
          }
        }
      }
    }
  }
  
  applyZoomTransformation(level) {
    if (!this.pixelBuffer || !level) return;
    
    const buffer = new Uint8Array(this.pixelBuffer);
    const centerX = this.width / 2;
    const centerY = this.height / 2;
    const zoom = level || 1.0;
    
    // Simple zoom transformation
    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        const dx = x - centerX;
        const dy = y - centerY;
        
        // Scale point
        const srcX = dx / zoom + centerX;
        const srcY = dy / zoom + centerY;
        
        // Check bounds and copy pixel
        if (srcX >= 0 && srcX < this.width && srcY >= 0 && srcY < this.height) {
          const srcIdx = (Math.floor(srcY) * this.width + Math.floor(srcX)) * 4;
          const dstIdx = (y * this.width + x) * 4;
          
          if (srcIdx >= 0 && srcIdx < buffer.length - 3) {
            this.pixelBuffer[dstIdx] = buffer[srcIdx];
            this.pixelBuffer[dstIdx + 1] = buffer[srcIdx + 1];
            this.pixelBuffer[dstIdx + 2] = buffer[srcIdx + 2];
            this.pixelBuffer[dstIdx + 3] = buffer[srcIdx + 3];
          }
        }
      }
    }
  }
  
  applyContrastTransformation(level) {
    if (!this.pixelBuffer) return;
    
    const contrast = level || 1.0;
    const factor = (259 * (contrast + 255)) / (255 * (259 - contrast));
    
    for (let i = 0; i < this.pixelBuffer.length; i += 4) {
      // Apply contrast to RGB channels
      this.pixelBuffer[i] = Math.max(0, Math.min(255, factor * (this.pixelBuffer[i] - 128) + 128));
      this.pixelBuffer[i + 1] = Math.max(0, Math.min(255, factor * (this.pixelBuffer[i + 1] - 128) + 128));
      this.pixelBuffer[i + 2] = Math.max(0, Math.min(255, factor * (this.pixelBuffer[i + 2] - 128) + 128));
      // Keep alpha unchanged
    }
  }
  
  applyBlurTransformation(radius) {
    if (!this.pixelBuffer) return;
    
    const blurRadius = Math.max(1, Math.floor(radius || 1));
    const tempBuffer = new Uint8Array(this.pixelBuffer);
    
    // Simple box blur implementation
    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        let r = 0, g = 0, b = 0, a = 0;
        let count = 0;
        
        // Sample surrounding pixels
        for (let dy = -blurRadius; dy <= blurRadius; dy++) {
          for (let dx = -blurRadius; dx <= blurRadius; dx++) {
            const nx = x + dx;
            const ny = y + dy;
            
            if (nx >= 0 && nx < this.width && ny >= 0 && ny < this.height) {
              const srcIndex = (ny * this.width + nx) * 4;
              r += tempBuffer[srcIndex];
              g += tempBuffer[srcIndex + 1];
              b += tempBuffer[srcIndex + 2];
              a += tempBuffer[srcIndex + 3];
              count++;
            }
          }
        }
        
        // Average and write back
        const dstIndex = (y * this.width + x) * 4;
        this.pixelBuffer[dstIndex] = Math.floor(r / count);
        this.pixelBuffer[dstIndex + 1] = Math.floor(g / count);
        this.pixelBuffer[dstIndex + 2] = Math.floor(b / count);
        this.pixelBuffer[dstIndex + 3] = Math.floor(a / count);
      }
    }
  }
  
  applyScrollTransformation(dx, dy) {
    if (!this.pixelBuffer) return;
    
    const buffer = new Uint8Array(this.pixelBuffer);
    const scrollX = Math.floor(dx || 0);
    const scrollY = Math.floor(dy || 0);
    
    // Clear buffer first
    this.pixelBuffer.fill(0);
    
    // Copy pixels with offset
    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        const srcX = x - scrollX;
        const srcY = y - scrollY;
        
        if (srcX >= 0 && srcX < this.width && srcY >= 0 && srcY < this.height) {
          const srcIdx = (srcY * this.width + srcX) * 4;
          const dstIdx = (y * this.width + x) * 4;
          
          this.pixelBuffer[dstIdx] = buffer[srcIdx];
          this.pixelBuffer[dstIdx + 1] = buffer[srcIdx + 1];
          this.pixelBuffer[dstIdx + 2] = buffer[srcIdx + 2];
          this.pixelBuffer[dstIdx + 3] = buffer[srcIdx + 3];
        }
      }
    }
  }

  async loadPiece(piecePath) {
    // Convert relative path to absolute file URL for ES module import
    const absolutePath = resolve(piecePath);
    const fileURL = pathToFileURL(absolutePath).href;
    
    // Load the piece module
    const pieceModule = await import(fileURL);
    console.log(`📦 Loaded piece with functions: ${Object.keys(pieceModule).join(', ')}`);
    
    // Check for AC disk structure (boot/sim/paint) vs simple api structure
    if (pieceModule.boot && pieceModule.paint) {
      console.log('🔧 AC disk detected - initializing with boot/sim/paint lifecycle...');
      
      // Initialize the disk with boot function
      if (pieceModule.boot) {
        const api = await this.createAPI();
        pieceModule.boot(api);
      }
      
      // Create a wrapper that handles sim + paint
      return (api) => {
        if (pieceModule.sim) {
          pieceModule.sim(api);
        }
        pieceModule.paint(api);
      };
    }
    
    // Handle simple api.paint structure
    if (!pieceModule.paint && !pieceModule.api?.paint) {
      throw new Error('Piece must export a paint function, api.paint, or AC disk structure (boot/paint)');
    }
    
    return pieceModule.paint || pieceModule.api?.paint;
  }

  async loadPieceModule(piecePath) {
    // Convert relative path to absolute file URL for ES module import
    const absolutePath = resolve(piecePath);
    const fileURL = pathToFileURL(absolutePath).href;
    
    // Load and return the raw piece module for separate sim/paint access
    const pieceModule = await import(fileURL);
    console.log(`📦 Loaded piece with functions: ${Object.keys(pieceModule).join(', ')}`);
    
    return pieceModule;
  }

  savePNG(basePath) {
    try {
      if (this.pixelBuffer) {
        const ts = timestamp();
        const filename = `${basePath}-${ts}.png`;
        const pngData = encodePNG(this.width, this.height, this.pixelBuffer);
        writeFileSync(filename, pngData);
        console.log(`🖼️ PNG saved to: ${filename}`);
        
        return { filename, timestamp: ts };
      } else {
        throw new Error('No pixel buffer available - AC system not properly initialized');
      }
    } catch (error) {
      console.error(chalk.red('💥 Error saving PNG:'), error);
      throw error;
    }
  }

  displayInTerminal() {
    console.log(`📺 Displaying ${this.width}x${this.height} pixels in terminal...`);
    
    try {
      const sixelData = pixelBufferToSixel(this.pixelBuffer, this.width, this.height, 2);
      
      // Output with a simple timeout mechanism
      const timeoutId = setTimeout(() => {
        console.log(chalk.yellow('\n⚠️ Sixel output timeout'));
      }, 1000);
      
      process.stdout.write(sixelData);
      clearTimeout(timeoutId);
      
    } catch (error) {
      console.log(chalk.yellow('⚠️ Sixel output error:', error.message));
    }
  }

  getStats() {
    const uniqueAPIs = [...new Set(this.apiCalls.map(call => call.name))];
    return {
      apiCalls: this.apiCalls.length,
      uniqueAPIs: uniqueAPIs.length,
      apis: uniqueAPIs
    };
  }

  // Support deterministic time injection and random seeding
  setSimulationTime(timeMs) {
    this.simulationTime = timeMs;
    
    // Create deterministic random based on simulation time
    this.deterministicRandom = this.createSeededRandom(timeMs);
    
    // Update API time functions to use simulation time
    if (this.api && this.api.time) {
      this.api.time = () => new Date(this.simulationTime);
    }
    if (this.api && this.api.clock) {
      // KidLisp expects api.clock.time() to return a Date object
      this.api.clock.time = () => new Date(this.simulationTime);
      console.log(`🕐 Clock API updated with simulation time: ${this.simulationTime}ms -> ${new Date(this.simulationTime).toISOString()}`);
    }
    if (this.api && this.api.perf) {
      this.api.perf = () => this.simulationTime;
    }
    
    // COMPREHENSIVE TIME OVERRIDE: Override global time functions that KidLisp might use
    if (this.api) {
      // Override Date.now and performance.now through the API
      this.api.Date = {
        now: () => this.simulationTime,
        ...Date
      };
      
      // Override performance timing
      this.api.performance = {
        now: () => this.simulationTime,
        ...performance
      };
      
      // Add simulation time as explicit API
      this.api.simulationTime = () => this.simulationTime;
      
      // Make sure all time APIs return consistent values
      this.api.now = () => this.simulationTime;
      this.api.timestamp = () => this.simulationTime;
    }
    
    // Update random functions to use deterministic random
    if (this.api && this.api.randInt) {
      this.api.randInt = (max) => Math.floor(this.deterministicRandom() * max);
    }
    if (this.api && this.api.randIntRange) {
      this.api.randIntRange = (min, max) => Math.floor(this.deterministicRandom() * (max - min + 1)) + min;
    }
  }

  // Print performance summary of graph operations
  printPerformanceSummary() {
    if (this.operationTimings.size === 0) {
      console.log('📊 No timing data recorded');
      return;
    }

    console.log('\n📊 Graph Operation Performance Summary:');
    console.log('=' .repeat(50));
    
    // Sort by total time spent
    const sortedOperations = Array.from(this.operationTimings.entries())
      .sort((a, b) => b[1].total - a[1].total);
    
    let totalTime = 0;
    for (const [operation, stats] of sortedOperations) {
      totalTime += stats.total;
    }
    
    for (const [operation, stats] of sortedOperations) {
      const avgTime = stats.total / stats.count;
      const percentage = ((stats.total / totalTime) * 100).toFixed(1);
      const avgStr = avgTime >= 1 ? `${avgTime.toFixed(2)}ms` : `${avgTime.toFixed(3)}ms`;
      const totalStr = stats.total >= 1 ? `${stats.total.toFixed(2)}ms` : `${stats.total.toFixed(3)}ms`;
      const maxStr = stats.max >= 1 ? `${stats.max.toFixed(2)}ms` : `${stats.max.toFixed(3)}ms`;
      const minStr = stats.min === Infinity ? '0.000ms' : (stats.min >= 1 ? `${stats.min.toFixed(2)}ms` : `${stats.min.toFixed(3)}ms`);
      
      console.log(`⚡ ${operation.padEnd(12)} | ${stats.count.toString().padStart(4)} calls | ${avgStr.padStart(8)} avg | ${totalStr.padStart(8)} total (${percentage}%) | max: ${maxStr} | min: ${minStr}`);
    }
    
    console.log('=' .repeat(50));
    const totalStr = totalTime >= 1 ? `${totalTime.toFixed(2)}ms` : `${totalTime.toFixed(3)}ms`;
    console.log(`💫 Total graph time: ${totalStr} across ${Array.from(this.operationTimings.values()).reduce((sum, stats) => sum + stats.count, 0)} operations`);
  }

  // Simple seeded random number generator (LCG)
  createSeededRandom(seed) {
    let state = seed % 2147483647;
    if (state <= 0) state += 2147483646;
    
    return function() {
      state = (state * 16807) % 2147483647;
      return (state - 1) / 2147483646;
    };
  }
}