// Complete headless AC environment with terminal sixel output
import { writeFileSync } from 'fs';
import { resolve } from 'path';
import { pathToFileURL } from 'url';
import { PNG } from 'pngjs';
import readline from 'readline';
import { timestamp } from "../../../system/public/aesthetic.computer/lib/num.mjs";
import chalk from 'chalk';
import ora from 'ora';
import cliProgress from 'cli-progress';
import boxen from 'boxen';
import figlet from 'figlet';

// Simple PNG encoding function
function encodePNG(width, height, pixelBuffer) {
  const png = new PNG({ width, height });
  
  // Copy pixel buffer to PNG data
  for (let i = 0; i < pixelBuffer.length; i++) {
    png.data[i] = pixelBuffer[i];
  }
  
  return PNG.sync.write(png);
}

// Mock browser APIs
function setupBrowserMocks() {
  global.performance = global.performance || {
    now: () => Date.now(),
    mark: () => {},
    measure: () => {},
    getEntriesByType: () => [],
    clearMarks: () => {},
    clearMeasures: () => {}
  };
  
  global.URL = global.URL || class MockURL {
    constructor(url) { this.href = url; this.searchParams = new URLSearchParams(); }
  };
  
  global.URLSearchParams = global.URLSearchParams || class MockURLSearchParams {
    constructor() { this.params = new Map(); }
    set(key, value) { this.params.set(key, value); }
    get(key) { return this.params.get(key); }
    has(key) { return this.params.has(key); }
    delete(key) { this.params.delete(key); }
  };
  
  global.fetch = global.fetch || (async () => {
    throw new Error('Fetch not supported in headless mode');
  });
  
  // Navigator is read-only in Node.js, so extend it if possible
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
      // If navigator is read-only, try to extend it
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
  
  global.window = global.window || {
    innerWidth: 800,
    innerHeight: 600,
    devicePixelRatio: 1,
    screen: { width: 800, height: 600 }
  };
  
  global.document = global.document || {
    createElement: () => ({ style: {}, addEventListener: () => {} }),
    body: { appendChild: () => {} },
    addEventListener: () => {},
    removeEventListener: () => {}
  };
  
  global.requestAnimationFrame = global.requestAnimationFrame || ((cb) => setTimeout(cb, 16));
  global.cancelAnimationFrame = global.cancelAnimationFrame || clearTimeout;
}

class FullHeadlessAC {
  constructor(width = 800, height = 600) {
    this.width = width;
    this.height = height;
    this.apiCalls = [];
    this.disk = null;
    this.graph = null;
    this.pixelBuffer = null;
    
    // Setup browser mocks
    setupBrowserMocks();
    
    console.log(chalk.cyan('🔧 Setting up full AC environment...'));
  }
  
  async initializeAC() {
    const spinner = ora('Loading AC system...').start();
    
    try {
      // Import graph first to set up the pixel buffer
      spinner.text = 'Loading graph.mjs...';
      const graphModule = await import("../../../system/public/aesthetic.computer/lib/graph.mjs");
      this.graph = graphModule;
      
      // Create pixel buffer and set it in graph
      this.pixelBuffer = new Uint8ClampedArray(this.width * this.height * 4);
      const buffer = {
        width: this.width,
        height: this.height,
        pixels: this.pixelBuffer
      };
      
      this.graph.setBuffer(buffer);
      spinner.text = 'Pixel buffer initialized...';
      
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
      
      spinner.succeed(chalk.green('🚀 AC system loaded successfully'));
      
    } catch (error) {
      spinner.fail(chalk.red('💥 Error loading AC system'));
      console.error(chalk.red(error));
      throw error;
    }
  }
  
  createAPI() {
    const self = this;
    
    // Throttling mechanism for live updates
    let lastUpdateTime = 0;
    const updateThrottle = 100; // Minimum 100ms between updates
    let pendingUpdate = false;
    
    function logCall(name, args) {
      self.apiCalls.push({ name, args: Array.from(args), timestamp: Date.now() });
      // Don't log individual calls - we'll show progress instead
    }
    
    function liveUpdate() {
      const now = Date.now();
      
      // If we're within the throttle window, schedule a delayed update
      if (now - lastUpdateTime < updateThrottle) {
        if (!pendingUpdate) {
          pendingUpdate = true;
          setTimeout(() => {
            executeUpdate();
            pendingUpdate = false;
          }, updateThrottle - (now - lastUpdateTime));
        }
        return;
      }
      
      executeUpdate();
    }
    
    function executeUpdate() {
      lastUpdateTime = Date.now();
      // Save cursor position, move to start of sixel area, show live update, restore cursor
      process.stdout.write('\x1b[s'); // Save cursor
      process.stdout.write('\x1b[H'); // Move to top
      process.stdout.write('\x1b[3J'); // Clear scrollback
      console.clear();
      console.log(chalk.cyan('🎨 Live Preview:'));
      self.displayInTerminal();
      process.stdout.write('\x1b[u'); // Restore cursor
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
          const foundColor = self.graph.findColor(...args);
          self.graph.color(...foundColor);
        }
        self.graph.clear();
        liveUpdate();
        return api;
      };
      
      api.ink = function(...args) {
        logCall('ink', args);
        const foundColor = self.graph.findColor(...args);
        self.graph.color(...foundColor);
        liveUpdate();
        return api;
      };
      
      api.line = function(...args) {
        logCall('line', args);
        self.graph.line(...args);
        liveUpdate();
        return api;
      };
      
      api.circle = function(...args) {
        logCall('circle', args);
        self.graph.circle(...args);
        liveUpdate();
        return api;
      };
      
      api.write = function(...args) {
        logCall('write', args);
        
        // Try to use the real AC write function from disk.mjs
        try {
          if (self.disk && typeof self.disk.default === 'function') {
            // Get the main $API object that has the write function
            const diskAPI = self.disk;
            
            // Try to find the write function in the loaded disk module
            if (diskAPI.write) {
              diskAPI.write.apply(api, args);
            } else {
              // Fallback to our simple text rendering
              if (args.length >= 3) {
                self.renderSimpleText(args[0], args[1], args[2]);
              }
            }
          } else {
            // Fallback to our simple text rendering
            if (args.length >= 3) {
              self.renderSimpleText(args[0], args[1], args[2]);
            }
          }
        } catch (error) {
          console.warn('⚠️ AC write function failed, using fallback:', error.message);
          if (args.length >= 3) {
            self.renderSimpleText(args[0], args[1], args[2]);
          }
        }
        
        liveUpdate();
        return api;
      };
      
      api.rect = function(...args) {
        logCall('rect', args);
        self.graph.box(...args);
        liveUpdate();
        return api;
      };
      
      api.box = function(...args) {
        logCall('box', args);
        self.graph.box(...args);
        liveUpdate();
        return api;
      };
      
      api.point = function(...args) {
        logCall('point', args);
        self.graph.point(...args);
        liveUpdate();
        return api;
      };
      
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
      
      wipe: (...args) => { logCall('wipe', args); return this; },
      ink: (...args) => { logCall('ink', args); return this; },
      line: (...args) => { logCall('line', args); return this; },
      circle: (...args) => { logCall('circle', args); return this; },
      write: (...args) => { logCall('write', args); return this; },
      rect: (...args) => { logCall('rect', args); return this; },
      box: (...args) => { logCall('box', args); return this; },
      point: (...args) => { logCall('point', args); return this; }
    };
  }
  
  renderSimpleText(text, x, y) {
    // Render text silently - no logging needed for clean progress display
    
    const charWidth = 8;
    const charHeight = 12;
    
    for (let i = 0; i < text.length; i++) {
      const charX = x + i * charWidth;
      
      // Draw a simple character block
      for (let dy = 0; dy < charHeight; dy++) {
        for (let dx = 0; dx < charWidth - 2; dx++) {
          // Simple pattern based on character
          const shouldDraw = (dy % 2 === 0) || (dx % 2 === 0) || 
                           (dy === 0) || (dy === charHeight - 1) ||
                           (dx === 0) || (dx === charWidth - 3);
          
          if (shouldDraw && this.graph && this.graph.point) {
            this.graph.point(charX + dx, y + dy);
          }
        }
      }
    }
  }
  
  async runPiece(piecePath, duration = 1000) {
    const ts = timestamp();
    console.log(`🚀 Running piece headless: ${piecePath} [${ts}]`);
    console.log(`📐 Canvas: ${this.width}x${this.height}, Duration: ${duration}ms`);
    
    try {
      // Convert relative path to absolute file URL for ES module import
      const absolutePath = resolve(piecePath);
      const fileURL = pathToFileURL(absolutePath).href;
      
      // Load the piece module
      const pieceModule = await import(fileURL);
      console.log(`📦 Loaded piece with functions: ${Object.keys(pieceModule).join(', ')}`);
      
      if (!pieceModule.paint && !pieceModule.api?.paint) {
        throw new Error('Piece must export a paint function or api.paint');
      }
      
      // Create API
      const api = this.createAPI();
      
      // Get the paint function
      const paintFn = pieceModule.paint || pieceModule.api?.paint;
      
      // Run paint function in a loop with npm-style progress
      console.log('🎨 Running paint loop...');
      
      const startTime = Date.now();
      let frameCount = 0;
      let lastProgressUpdate = 0;
      
      while (Date.now() - startTime < duration) {
        try {
          await paintFn(api);
          frameCount++;
          
          // Update progress inline every 50ms (npm-style frequency)
          const elapsed = Date.now() - startTime;
          if (elapsed - lastProgressUpdate > 50) {
            const progress = Math.min(100, (elapsed / duration) * 100);
            const uniqueAPIs = [...new Set(this.apiCalls.map(call => call.name))];
            
            // Clean npm-style progress bar using readline
            const barLength = 30;
            const filled = Math.round((progress / 100) * barLength);
            const bar = '█'.repeat(filled) + '░'.repeat(barLength - filled);
            
            // Use readline for proper line clearing
            readline.clearLine(process.stdout, 0);
            readline.cursorTo(process.stdout, 0);
            process.stdout.write(`[${bar}] ${progress.toFixed(1)}% • ${frameCount} frames • ${this.apiCalls.length} calls • ${uniqueAPIs.length} APIs`);
            lastProgressUpdate = elapsed;
          }
        } catch (error) {
          console.error('\n💥 Error in paint function:', error);
          break;
        }
        
        // Throttle to ~60fps
        await new Promise(resolve => setTimeout(resolve, 16));
      }
      
      // Final completion line (npm-style) using readline
      const uniqueAPIs = [...new Set(this.apiCalls.map(call => call.name))];
      readline.clearLine(process.stdout, 0);
      readline.cursorTo(process.stdout, 0);
      console.log(`[████████████████████████████████] 100.0% • ${frameCount} frames • ${this.apiCalls.length} calls • ${uniqueAPIs.length} APIs`);
      
      return {
        success: true,
        frames: frameCount,
        apiCalls: this.apiCalls.length,
        uniqueAPIs: [...new Set(this.apiCalls.map(call => call.name))].length,
        apisUsed: [...new Set(this.apiCalls.map(call => call.name))],
        timestamp: ts
      };
      
    } catch (error) {
      console.error('💥 Error running piece:', error);
      return {
        success: false,
        error: error.message,
        apiCalls: this.apiCalls.length,
        uniqueAPIs: [...new Set(this.apiCalls.map(call => call.name))].length,
        apisUsed: [...new Set(this.apiCalls.map(call => call.name))],
        timestamp: ts
      };
    }
  }
  
  savePNG(baseFilename) {
    const ts = timestamp();
    const filename = `${baseFilename}-${ts}.png`;
    
    try {
      // Use the actual pixel buffer from graph.mjs
      if (this.pixelBuffer) {
        const pngData = encodePNG(this.width, this.height, this.pixelBuffer);
        writeFileSync(filename, pngData);
        console.log(`🖼️ PNG saved to: ${filename}`);
        
        // Also display in terminal
        this.displayInTerminal();
        
        return { filename, timestamp: ts };
      } else {
        throw new Error('No pixel buffer available - AC system not properly initialized');
      }
    } catch (error) {
      console.error('💥 Error saving PNG:', error);
      return { error: error.message, timestamp: ts };
    }
  }
  
  // Display pixel buffer in terminal using sixel graphics (inline)
  displayInTerminal() {
    console.log(`📺 Displaying ${this.width}x${this.height} pixels in terminal...`);
    
    // Convert pixel buffer to sixel format
    const sixelData = this.pixelBufferToSixel();
    
    // Output sixel data inline (no cursor movement)
    process.stdout.write(sixelData);
    process.stdout.write('\n\n'); // Add some spacing after the image
  }

  // Convert pixel buffer to sixel format using full-color palette with 2x scaling
  pixelBufferToSixel() {
    const width = this.width;
    const height = this.height;
    const scale = 2; // 2x scaling like sixel examples
    
    // Start sixel sequence
    let output = '\x1bPq'; // DCS + sixel start
    
    // Dynamic color palette generation (like ultra-fast-sixel.mjs)
    const colors = new Map();
    let colorIndex = 0;
    
    // Pre-allocate band arrays to avoid repeated allocation
    const bandArrays = new Map();
    
    // Process scaled pixels in groups of 6 rows (sixel height)
    const scaledHeight = height * scale;
    
    for (let band = 0; band < Math.ceil(scaledHeight / 6); band++) {
      bandArrays.clear();
      
      for (let scaledX = 0; scaledX < width * scale; scaledX++) {
        for (let dy = 0; dy < 6; dy++) {
          const scaledY = band * 6 + dy;
          if (scaledY >= scaledHeight) break;
          
          // Map scaled coordinates back to original pixel buffer
          const originalX = Math.floor(scaledX / scale);
          const originalY = Math.floor(scaledY / scale);
          
          const pixelIndex = (originalY * width + originalX) * 4;
          const r = this.pixelBuffer[pixelIndex];
          const g = this.pixelBuffer[pixelIndex + 1];
          const b = this.pixelBuffer[pixelIndex + 2];
          const a = this.pixelBuffer[pixelIndex + 3];
          
          // Skip transparent pixels
          if (a === 0) continue;
          
          // Create color key to group identical colors
          const colorKey = (r << 16) | (g << 8) | b;
          
          // Register new color if not seen before
          if (!colors.has(colorKey)) {
            colors.set(colorKey, colorIndex++);
            // Add color definition to output (RGB percentages 0-100)
            output += `#${colors.get(colorKey)};2;${Math.round(r*100/255)};${Math.round(g*100/255)};${Math.round(b*100/255)}`;
          }
          
          const color = colors.get(colorKey);
          if (!bandArrays.has(color)) {
            bandArrays.set(color, new Array(width * scale).fill(0));
          }
          bandArrays.get(color)[scaledX] |= (1 << dy);
        }
      }
      
      // Output band data for each color
      for (const [color, pixels] of bandArrays) {
        output += `#${color}`;
        for (const pixel of pixels) {
          output += String.fromCharCode(63 + pixel);
        }
        output += '$'; // End of line
      }
      output += '-'; // Next sixel row
    }
    
    output += '\x1b\\'; // End sixel sequence
    return output;
  }

  // Remove the old simple color matching function
  matchColor(r, g, b) {
    // This is no longer needed since we use dynamic color palette
    return 0;
  }
}

// CLI usage
async function main() {
  // Clear screen and show header
  console.clear();
  console.log(chalk.cyan(figlet.textSync('AESTHETIC', { font: 'Small' })));
  console.log(chalk.gray('Headless Computer • Terminal Graphics Engine\n'));
  
  const args = process.argv.slice(2);
  const piecePath = args[0];
  const duration = parseInt(args[1]) || 1000;
  
  if (!piecePath) {
    console.error(chalk.red('Usage: node full-headless.mjs <piece-path> [duration-ms]'));
    process.exit(1);
  }
  
  console.log(chalk.blue(`🎬 Target piece: ${piecePath}`));
  console.log(chalk.blue(`⏱️  Duration: ${duration}ms\n`));
  
  // Create full headless AC environment with 128x128 canvas
  const headless = new FullHeadlessAC(128, 128);
  
  // Wait for AC to initialize
  await headless.initializeAC();
  
  // Run the piece
  const result = await headless.runPiece(piecePath, duration);
  
  // Save timestamped output
  const saveResult = headless.savePNG('/workspaces/aesthetic-computer/reference/tools/output/headless-full');
  
  // Final output with boxen
  const summary = `${chalk.green('✅ Rendering Complete!')}\n\n` +
                 `${chalk.cyan('Success:')} ${result.success}\n` +
                 `${chalk.cyan('Frames:')} ${result.frames || 0}\n` +
                 `${chalk.cyan('API Calls:')} ${result.apiCalls}\n` +
                 `${chalk.cyan('Unique APIs:')} ${result.uniqueAPIs}\n` +
                 `${chalk.cyan('APIs Used:')} ${result.apisUsed?.join(', ') || 'none'}\n` +
                 `${chalk.cyan('Output:')} ${saveResult.filename || saveResult.error}`;
  
  console.log('\n' + boxen(summary, {
    padding: 1,
    margin: 1,
    borderStyle: 'round',
    borderColor: 'cyan'
  }));
  
  if (!result.success) {
    console.error(chalk.red(`\n💥 Error: ${result.error}`));
    process.exit(1);
  }
  
  // Force clean exit to prevent hanging
  process.exit(0);
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { FullHeadlessAC };