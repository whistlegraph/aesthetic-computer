#!/usr/bin/env node

/**
 * Stateless Frame Renderer
 * Renders exactly one frame then exits
 * Can resume from any point by reading state.json
 */

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { HeadlessAC } from "./headless.mjs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

class FrameRenderer extends HeadlessAC {
  constructor(outputDir, width = 2048, height = 2048, density = null) {
    // Use provided dimensions or fall back to environment variables
    width = width || parseInt(process.env.WIDTH) || 2048;
    height = height || parseInt(process.env.HEIGHT) || 2048;
    super(width, height, { density, outputDir });
    this.outputDir = outputDir;
    this.stateFile = path.join(outputDir, 'state.json');
    this.backgroundBufferFile = path.join(outputDir, 'background-buffer.bin');
    this.density = density;
    
    // State containers for serialization/deserialization
    this.pieceState = {}; // Variables from the piece itself
    this.storeData = {}; // AC store data
    this.kidlispState = {}; // KidLisp instance state including onceExecuted
    this.pieceModule = null; // Cache the piece module
    this.diskState = null; // State from AC disk lifecycle
    this.backgroundBuffer = null; // Previous frame's pixel buffer for continuity
  }

    // Load or create initial state
  loadState() {
    if (fs.existsSync(this.stateFile)) {
      console.log(`🔄 Resuming from existing state`);
      const state = JSON.parse(fs.readFileSync(this.stateFile, 'utf8'));
      
      // Restore piece state if it exists
      if (state.pieceState) {
        console.log(`🧩 Restoring piece state with ${Object.keys(state.pieceState).length} variables`);
        this.pieceState = state.pieceState;
      }
      
      // Restore AC store data if it exists
      if (state.storeData) {
        console.log(`💾 Restoring store data with ${Object.keys(state.storeData).length} entries`);
        this.storeData = state.storeData;
      }
      
      // Restore KidLisp state if it exists
      if (state.kidlispState) {
        console.log(`🎨 Restoring KidLisp state with ${Object.keys(state.kidlispState).length} properties`);
        this.kidlispState = state.kidlispState;
      }
      
      // Note: Background buffer will be loaded in renderFrame after canvas is sized
      
      return state;
    } else {
      console.log(`🆕 Starting new render sequence`);
      const pieceArg = process.argv[2] || 'test-piece.mjs';
      const totalFrames = parseInt(process.argv[3]) || 300; // Default to 300 frames if not provided
      
      return {
        frameIndex: 0,
        startTime: Date.now(),
        piece: pieceArg,
        duration: totalFrames * (1000/60), // Convert frames to milliseconds for compatibility
        fps: 60,
        width: this.width,
        height: this.height,
        totalFrames: totalFrames,
        pieceState: {},
        storeData: {},
        kidlispState: {}
      };
    }
  }

  // Load background buffer from previous frame
  loadBackgroundBuffer() {
    this.backgroundBufferRestored = false; // Reset flag
    if (fs.existsSync(this.backgroundBufferFile)) {
      console.log(`🖼️ Restoring background buffer from previous frame`);
      try {
        const startTime = Date.now();
        const bufferData = fs.readFileSync(this.backgroundBufferFile);
        
        // Ensure the buffer is the right size
        if (bufferData.length === this.pixelBuffer.length) {
          this.pixelBuffer.set(bufferData);
          this.backgroundBufferRestored = true; // Set flag when successfully restored
          const loadTime = Date.now() - startTime;
          console.log(`✅ Background buffer restored (${bufferData.length} bytes in ${loadTime}ms)`);
          console.log(`🧪 Background buffer preview (first 16 bytes):`, Array.from(this.pixelBuffer.slice(0, 16)));
        } else {
          console.log(`⚠️ Background buffer size mismatch: expected ${this.pixelBuffer.length}, got ${bufferData.length}`);
        }
      } catch (error) {
        console.log(`⚠️ Failed to load background buffer: ${error.message}`);
      }
    }
  }

  // Save background buffer for next frame
  saveBackgroundBuffer() {
    try {
      const startTime = Date.now();
      console.log(`🧪 Background buffer sample before save (first 16 bytes):`, Array.from(this.pixelBuffer.slice(0, 16)));
      fs.writeFileSync(this.backgroundBufferFile, this.pixelBuffer);
      const saveTime = Date.now() - startTime;
      console.log(`💾 Background buffer saved for frame continuity (${saveTime}ms)`);
    } catch (error) {
      console.log(`⚠️ Failed to save background buffer: ${error.message}`);
    }
  }

  // Save state for next frame
  saveState(state) {
    // Include piece state, store data, and KidLisp state in the state object
    const completeState = {
      ...state,
      pieceState: this.pieceState || {},
      storeData: this.storeData || {},
      kidlispState: this.getKidlispState()
    };
    
    fs.writeFileSync(this.stateFile, JSON.stringify(completeState, null, 2));
  }

  // Check if rendering is complete
  isComplete(state) {
    return state.frameIndex >= state.totalFrames;
  }

  // Render one frame using the existing headless system
  async renderFrame(state) {
    const frameStartTime = Date.now();
    console.log(`🎬 Rendering frame ${state.frameIndex}/${state.totalFrames}`);
    
    // Calculate frame time
    const frameTimeMs = (state.frameIndex / state.fps) * 1000;

    // Keep frame counters synchronized for downstream helpers
    this.frameCount = state.frameIndex;
    
    // Set the simulation time for timing-based expressions
    this.setSimulationTime(frameTimeMs);
    
    // Update canvas size if needed
    if (this.width !== state.width || this.height !== state.height) {
      console.log(`🖼️ Resizing canvas from ${this.width}x${this.height} to ${state.width}x${state.height}`);
      this.width = state.width;
      this.height = state.height;
      this.pixelBuffer = new Uint8Array(state.width * state.height * 4);
      
      // Initialize to black if this is a new canvas size
      for (let i = 0; i < this.pixelBuffer.length; i += 4) {
        this.pixelBuffer[i] = 0;     // R
        this.pixelBuffer[i + 1] = 0; // G
        this.pixelBuffer[i + 2] = 0; // B
        this.pixelBuffer[i + 3] = 255; // A (opaque)
      }
      
      // Force API recreation after resize to get correct screen dimensions
      this.api = null;
    }
    
    // Load background buffer from previous frame for continuity (only after frame 0)
    let backgroundLoadTime = 0;
    if (state.frameIndex > 0) {
      const bgStartTime = Date.now();
      this.loadBackgroundBuffer();
      backgroundLoadTime = Date.now() - bgStartTime;
      
      // CRITICAL: Update graph buffer reference after loading background buffer
      // The graph.mjs system needs to point to the updated pixelBuffer with restored pixels
      if (this.graph && this.graph.setBuffer) {
        const buffer = {
          width: this.width,
          height: this.height,
          pixels: this.pixelBuffer  // This now contains the restored background pixels
        };
        this.graph.setBuffer(buffer);
        console.log('🎨 Updated graph buffer reference with restored background pixels');
      }
    }
    
    // Initialize AC system if not already done
    let acInitTime = 0;
    let apiCreateTime = 0;
    if (!this.api) {
      const acStartTime = Date.now();
      await this.initializeAC();
      acInitTime = Date.now() - acStartTime;
      
      // Set KidLisp state BEFORE creating API so it's available when KidLisp instance is created
      if (state.kidlispState) {
        this.setKidlispState(state.kidlispState);
        console.log('🔄 Pre-set KidLisp state before API creation');
      }
      
      const apiStartTime = Date.now();
      this.api = await this.createAPI(); // Create the API after initialization - screen.pixels will now reference the loaded background buffer
      apiCreateTime = Date.now() - apiStartTime;
        
      if (this.kidlispInstance) {
        console.log(`🎯 KidLisp instance established at frame ${state.frameIndex}`);
        
        // CRITICAL: Ensure simulation time is properly set for KidLisp timing calculations
        // Force update the time after API creation to ensure consistent timing
        this.setSimulationTime(frameTimeMs);
        console.log(`🕐 Re-synchronized simulation time after API creation: ${frameTimeMs}ms`);
      }
    }

    // Keep KidLisp timing counters aligned with renderer frame index
    if (this.kidlispInstance) {
      this.kidlispInstance.frameCount = state.frameIndex;
    }
    
    // Load the piece module if not cached
    let pieceLoadTime = 0;
    if (!this.pieceModule) {
      const pieceStartTime = Date.now();
      
      // Check if this is a KidLisp code (starts with $)
      console.log(`🔍 Debug: piece="${state.piece}", GIF_MODE=${process.env.GIF_MODE}`);
      if (state.piece.startsWith('$') && process.env.GIF_MODE) {
        console.log(`🤖 KidLisp code detected: ${state.piece}`);
        
        // Check if temp file already exists (for subsequent frames)
        const tempPath = path.join(this.outputDir, 'temp-kidlisp.mjs');
        if (fs.existsSync(tempPath)) {
          console.log(`🔄 Reusing existing KidLisp wrapper`);
          this.pieceModule = await import(path.resolve(tempPath) + '?t=' + Date.now());
        } else {
          // Create a temporary wrapper piece for KidLisp code
          const kidlispWrapper = `
// Temporary KidLisp wrapper for ${state.piece}
export const paint = ($) => {
  // TODO: Resolve KidLisp code and execute it
  // For now, create a simple animated visual
  const t = $.api.frame / 60;
  $.wipe('purple');
  $.ink('white');
  $.line(200 + Math.sin(t) * 100, 200, 200 + Math.cos(t) * 100, 200);
};
`;
          
          // Write temporary file
          fs.writeFileSync(tempPath, kidlispWrapper);
          this.pieceModule = await import(path.resolve(tempPath));
          console.log(`🤖 Created KidLisp wrapper: ${Object.keys(this.pieceModule).join(', ')}`);
        }
        
      } else {
        // Try to import the piece - handle both with and without .mjs extension
        let piecePath = state.piece;
        if (!piecePath.endsWith('.mjs') && !piecePath.endsWith('.js')) {
          piecePath = `${piecePath}.mjs`;
        }
        this.pieceModule = await import(path.resolve(piecePath));
        console.log(`📦 Loaded piece with functions: ${Object.keys(this.pieceModule).join(', ')}`);
      }
      
      pieceLoadTime = Date.now() - pieceStartTime;
    }
    
    // Load and run the piece
    let pieceExecutionTime = 0;
    try {
      const pieceExecStartTime = Date.now();
      
      // Restore piece state to global scope (for pieces that use global variables)
      if (state.pieceState) {
        Object.assign(global, state.pieceState);
        console.log(`🧩 Restored ${Object.keys(state.pieceState).length} piece variables to global scope`);
      }
      
      // Create enhanced API with state persistence and frame injection
      const enhancedAPI = {
        ...this.api,
        
        // Frame context for pieces (both frameIndex and frame for compatibility)
        frameIndex: state.frameIndex,
        frame: state.frameIndex, // KidLisp expects api.frame
        paintCount: state.frameIndex, // KidLisp frame function expects api.paintCount
        frameTime: frameTimeMs,
        totalFrames: state.totalFrames,
        
        // Store system for persistence
        store: {
          ...this.storeData,
          persist: (key, method = "memory") => {
            // Store in our state system
            console.log(`💾 Persisting ${key} to ${method}`);
          }
        },
        
        // Simulation count (for pieces that depend on it)
        simCount: BigInt(state.frameIndex),
        
        // Add common injection patterns
        $: this.api // Common alias
      };

      // Create piece parameters object (for pieces that expect destructured params)
      const pieceParams = {
        api: enhancedAPI,
        frameIndex: state.frameIndex,
        frameTime: frameTimeMs,
        totalFrames: state.totalFrames,
        simCount: BigInt(state.frameIndex)
      };
      
      // Clear the canvas (only for frame 0 - background persistence preserves previous frames)
      if (state.frameIndex === 0) {
        this.pixelBuffer.fill(0);
        for (let i = 3; i < this.pixelBuffer.length; i += 4) {
          this.pixelBuffer[i] = 255; // Alpha
        }
        console.log('🎨 Canvas cleared for frame 0');
      } else {
        console.log('🎨 Preserving background buffer for frame continuity');
      }
      
      // Handle AC disk structure (boot/sim/paint) vs simple paint function
      if (this.pieceModule.boot && this.pieceModule.paint) {
        console.log('🔧 AC disk detected - running boot/sim/paint lifecycle...');
        
        // Only run boot on the first frame
        if (state.frameIndex === 0) {
          console.log('🥾 Running boot function...');
          await this.pieceModule.boot(pieceParams);
        }
        
        // Run sim function if it exists
        if (this.pieceModule.sim) {
          await this.pieceModule.sim(enhancedAPI, {
            frameIndex: state.frameIndex,
            frameTime: frameTimeMs,
            totalFrames: state.totalFrames,
            simCount: BigInt(state.frameIndex)
          });
        }
        
        // Run paint function
        await this.pieceModule.paint(enhancedAPI, {
          frameIndex: state.frameIndex,
          frameTime: frameTimeMs,
          totalFrames: state.totalFrames,
          simCount: BigInt(state.frameIndex)
        });
        
      } else if (this.pieceModule.paint) {
        // Standard paint function only
        await this.pieceModule.paint(enhancedAPI, {
          frameIndex: state.frameIndex,
          frameTime: frameTimeMs,
          totalFrames: state.totalFrames,
          simCount: BigInt(state.frameIndex)
        });
        
      } else if (this.pieceModule.default && typeof this.pieceModule.default === 'function') {
        // Default export function
        await this.pieceModule.default(enhancedAPI, {
          frameIndex: state.frameIndex,
          frameTime: frameTimeMs,
          totalFrames: state.totalFrames,
          simCount: BigInt(state.frameIndex)
        });
        
      } else {
        throw new Error(`Piece ${state.piece} does not export valid AC disk structure or paint function`);
      }
      
      // Capture piece state from global scope after execution
      const capturedState = {};
      
      // Common piece variables to capture
      const commonStateVars = [
        'frameCount', 'marqueeOffset', 'currentFrame', 'animationState', 
        'time', 'progress', 'phase', 'cycle', 'step', 'counter', 'index',
        'x', 'y', 'vx', 'vy', 'angle', 'rotation', 'scale', 'opacity',
        'colors', 'positions', 'velocities', 'particles', 'objects',
        'gameState', 'score', 'level', 'lives', 'health', 'energy',
        'stars', 'bouncingStars' // elcid-flyer specific state
      ];
      
      // Capture variables that exist in global scope
      commonStateVars.forEach(varName => {
        if (typeof global[varName] !== 'undefined') {
          capturedState[varName] = global[varName];
        }
      });
      
      // Store captured state
      this.pieceState = capturedState;
      
      // Capture store data that was modified
      this.storeData = { ...enhancedAPI.store };
      
      pieceExecutionTime = Date.now() - pieceExecStartTime;
      console.log(`📊 Captured ${Object.keys(capturedState).length} piece variables`);
      
      // CRITICAL: Sync API screen buffer back to our pixel buffer BEFORE capturing frame
      if (this.api && this.api.screen && this.api.screen.pixels) {
        console.log('🔄 Syncing API screen buffer to pixel buffer for persistence');
        // Debug: Check a few pixels to see if there's actual content
        const samplePixels = [];
        for (let i = 0; i < Math.min(20, this.api.screen.pixels.length); i += 4) {
          samplePixels.push(`[${this.api.screen.pixels[i]},${this.api.screen.pixels[i+1]},${this.api.screen.pixels[i+2]},${this.api.screen.pixels[i+3]}]`);
        }
        console.log('🔍 API screen buffer sample pixels:', samplePixels.slice(0, 5).join(', '));
        
        // Also check if graph buffer has different content
        if (this.graph && this.graph.pixels) {
          const graphSamplePixels = [];
          for (let i = 0; i < Math.min(20, this.graph.pixels.length); i += 4) {
            graphSamplePixels.push(`[${this.graph.pixels[i]},${this.graph.pixels[i+1]},${this.graph.pixels[i+2]},${this.graph.pixels[i+3]}]`);
          }
          console.log('🔍 Graph buffer sample pixels:', graphSamplePixels.slice(0, 5).join(', '));
        }
        
        this.pixelBuffer.set(this.api.screen.pixels);
      }
      
      // Capture the frame AFTER syncing the screen buffer
      const frameData = this.captureFrame();
      
      // Convert RGBA to RGB for ffmpeg
      const rgbData = new Uint8Array(state.width * state.height * 3);
      for (let i = 0; i < frameData.length; i += 4) {
        const rgbIndex = (i / 4) * 3;
        rgbData[rgbIndex] = frameData[i];     // R
        rgbData[rgbIndex + 1] = frameData[i + 1]; // G
        rgbData[rgbIndex + 2] = frameData[i + 2]; // B
        // Skip alpha channel
      }
      
      // Write frame file
      const fileWriteStartTime = Date.now();
      const frameFile = path.join(this.outputDir, `frame-${String(state.frameIndex).padStart(6, '0')}.rgb`);
      fs.writeFileSync(frameFile, rgbData);
      const fileWriteTime = Date.now() - fileWriteStartTime;
      
      console.log(`✅ Frame ${state.frameIndex} written (${rgbData.length} bytes)`);
      
      // Save background buffer for next frame continuity
      this.saveBackgroundBuffer();
      
      // Performance summary
      const totalFrameTime = Date.now() - frameStartTime;
      console.log(`⏱️ FRAME ${state.frameIndex} TIMING: Total=${totalFrameTime}ms | BG=${backgroundLoadTime}ms | AC=${acInitTime}ms | API=${apiCreateTime}ms | Piece=${pieceLoadTime}ms | Exec=${pieceExecutionTime}ms | File=${fileWriteTime}ms`);
      
      return {
        ...state,
        frameIndex: state.frameIndex + 1,
        lastFrameTime: frameTimeMs,
        pieceState: this.pieceState,
        storeData: this.storeData
      };
      
    } catch (error) {
      console.error(`💥 Frame ${state.frameIndex} render failed:`, error.message);
      throw error;
    }
  }

  // Capture frame method (inherited from HeadlessAC but make sure it exists)
  captureFrame() {
    return new Uint8Array(this.pixelBuffer);
  }

  async run() {
    try {
      // Ensure output directory exists
      fs.mkdirSync(this.outputDir, { recursive: true });
      
      // Load current state
      const currentState = this.loadState();
      
      // Check if complete
      if (this.isComplete(currentState)) {
        console.log(`🎯 Rendering complete! ${currentState.frameIndex} frames rendered.`);
        return { complete: true, state: currentState };
      }
      
      // Render exactly one frame
      const newState = await this.renderFrame(currentState);
      
      // Save state for next invocation
      this.saveState(newState);
      
      console.log(`📝 State saved. Next frame: ${newState.frameIndex}`);
      
      return { complete: this.isComplete(newState), state: newState };
      
    } catch (error) {
      console.error(`💥 Frame render failed:`, error);
      process.exit(1);
    }
  }
}

// CLI usage
if (import.meta.url === `file://${process.argv[1]}`) {
  const outputDir = process.argv[4] || './frame-output';
  const width = parseInt(process.argv[5]) || 2048;
  const height = parseInt(process.argv[6]) || 2048;
  const density = process.argv[7] ? parseFloat(process.argv[7]) : null;
  const renderer = new FrameRenderer(outputDir, width, height, density);
  
  renderer.run().then(result => {
    if (result.complete) {
      console.log(`🏁 All frames complete!`);
    } else {
      console.log(`🔄 Frame done, process exiting cleanly`);
    }
    process.exit(0);
  });
}

export { FrameRenderer };