// Headless AC - Shared functionality for bake.mjs and tape.mjs
// Provides common AC system initialization, API creation, and utilities

import { writeFileSync, readFileSync } from 'fs';
import { resolve } from 'path';
import { pathToFileURL } from 'url';
import { PNG } from 'pngjs';
import { timestamp } from "../../../system/public/aesthetic.computer/lib/num.mjs";
import chalk from 'chalk';
import ora from 'ora';

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
    
    // Performance tracking options
    this.detailedTiming = options.detailedTiming || false;
    this.operationTimings = new Map();
    
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

  // Enable V8 optimizations for performance
  enableV8Optimizations() {
    // Hint to V8 that these are hot functions
    if (global.gc) {
      console.log('💫 V8 optimizations: Garbage collection available');
    }
    
    // Pre-allocate some objects to avoid GC pressure
    this.scratchBuffer = new Uint8Array(this.width * this.height * 4);
    this.colorCache = new Map();
    
    console.log(`🚀 Performance mode: Pixel buffer ${this.width}x${this.height} (${(this.pixelBuffer.length / 1024 / 1024).toFixed(2)}MB)`);
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
      
      global.location = global.location || {
        href: 'http://localhost:8888',
        origin: 'http://localhost:8888',
        protocol: 'http:',
        host: 'localhost:8888',
        pathname: '/',
        search: '',
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
        console.log(chalk.green('✅ Text module loaded:'), Object.keys(textModule));
      } catch (error) {
        console.log(chalk.yellow('⚠️ Text module not found:', error.message));
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
            // Convert AC path to actual file path
            const actualPath = path.replace('aesthetic.computer/', '../../../system/public/aesthetic.computer/');
            
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
        console.log(chalk.green('✅ Font system loaded with glyphs:'), Object.keys(this.typeface.glyphs).length, 'characters');
        
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
  
  createAPI() {
    const self = this;
    
    function logCall(name, args) {
      self.apiCalls.push({ name, args: Array.from(args), timestamp: Date.now() });
    }

    // Performance timing wrapper for graph operations
    function timeGraphOperation(operationName, graphFunc, ...args) {
      const startTime = process.hrtime.bigint(); // Use high-resolution time
      const result = graphFunc(...args);
      const endTime = process.hrtime.bigint();
      const duration = Number(endTime - startTime) / 1000000; // Convert nanoseconds to milliseconds
      
      // Log if operation takes longer than 0.01ms or if detailed logging is enabled
      if (duration > 0.01 || self.detailedTiming) {
        console.log(`⚡ ${operationName}: ${duration.toFixed(3)}ms`);
      }
      
      // Track total time per operation type
      if (!self.operationTimings) self.operationTimings = new Map();
      const existing = self.operationTimings.get(operationName) || { total: 0, count: 0, max: 0, min: Infinity };
      existing.total += duration;
      existing.count += 1;
      existing.max = Math.max(existing.max, duration);
      existing.min = Math.min(existing.min, duration);
      self.operationTimings.set(operationName, existing);
      
      return result;
    }

    // Try to use real disk.mjs API if available
    if (this.disk && this.graph) {
      console.log('🔧 Creating graph-based API...');
      
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
        timeGraphOperation('clear', self.graph.clear.bind(self.graph));
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
            let x, y, text, pos = {};
            if (args.length >= 3) {
              [text, x, y] = args;
              pos = { x, y };
            } else if (args.length === 2) {
              [text, pos] = args;
            } else if (args.length === 1) {
              text = args[0];
            }
            
            // Call the real typeface print method
            self.typeface.print(mockAPI, pos, 0, text);
            
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
        console.log(`🌀 Blur: ${args.join(', ')}`);
        
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
        console.log(`📜 Scroll: ${args.join(', ')}`);
        
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
          self.graph.flood(...args);
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
          }
          console.log(`📄 Switched to buffer: ${buffer.width}x${buffer.height}`);
        } else {
          console.warn('⚠️ page() called with invalid buffer:', buffer);
        }
      };
      
      // Write function for text rendering (used by embedded KidLisp pieces)
      api.write = function(...args) {
        logCall('write', args);
        console.log(`📝 Write: ${args[0] || ''}`);
        
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
            let x, y, text, pos = {};
            if (args.length >= 3) {
              [text, x, y] = args;
              pos = { x, y };
            } else if (args.length === 2) {
              [text, pos] = args;
            } else if (args.length === 1) {
              text = args[0];
            }
            
            // Call the real typeface print method
            self.typeface.print(mockAPI, pos, 0, text);
            
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
        time: () => new Date()
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
        const api = this.createAPI();
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
      this.api.clock = () => this.simulationTime;
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