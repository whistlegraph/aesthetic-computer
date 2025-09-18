#!/usr/bin/env node

/**
 * Blur Stress Test - Profile blur function under sustained load
 * This test simulates the conditions that cause blur crashes in long recordings
 */

import { performance } from 'perf_hooks';
import { writeFileSync } from 'fs';

// Mock the graph module environment
let width = 2048;
let height = 2048;
let pixels = new Uint8ClampedArray(width * height * 4);

// Blur-related globals (from graph.mjs)
let blurBuffer1 = null;
let blurBuffer2 = null;
let blurFrameCount = 0;
let blurCache = new Map();
let activeMask = null;

// Mock global functions
global.performance = performance;
global.console = console;
globalThis.dashboardLog = (type, message) => {
  console.log(`[${type.toUpperCase()}] ${message}`);
};

// Recording mode detection
function isInRecordingMode() {
  return true; // Always in recording mode for this test
}

// Generate noise pattern for testing
function generateNoiseBuffer(frame) {
  console.log(`🎨 Generating noise pattern for frame ${frame}`);
  
  // Create varied noise patterns to stress different blur scenarios
  const seed = frame * 1337; // Deterministic but varied
  
  for (let i = 0; i < pixels.length; i += 4) {
    const x = (i / 4) % width;
    const y = Math.floor((i / 4) / width);
    
    // Create different patterns based on frame
    let r, g, b, a;
    
    if (frame % 10 < 3) {
      // High-frequency noise (hard for blur)
      r = (Math.sin(x * 0.1 + frame * 0.05) * 127 + 128) | 0;
      g = (Math.cos(y * 0.1 + frame * 0.03) * 127 + 128) | 0;
      b = (Math.sin((x + y) * 0.05 + frame * 0.07) * 127 + 128) | 0;
      a = 255;
    } else if (frame % 10 < 6) {
      // Gradient pattern (easier for blur)
      r = (x / width * 255) | 0;
      g = (y / height * 255) | 0;
      b = ((x + y) / (width + height) * 255) | 0;
      a = 255;
    } else {
      // Mixed pattern with transparency
      const noise = Math.sin(x * 0.01 + y * 0.01 + frame * 0.1);
      r = (noise > 0 ? 255 : 0);
      g = (Math.cos(x * 0.02 + frame * 0.02) * 127 + 128) | 0;
      b = (Math.sin(y * 0.02 + frame * 0.03) * 127 + 128) | 0;
      a = (Math.abs(noise) * 255) | 0;
    }
    
    pixels[i] = r;
    pixels[i + 1] = g;
    pixels[i + 2] = b;
    pixels[i + 3] = a;
  }
}

// Blur weight generation (from graph.mjs)
function generateBlurWeights(radius) {
  const sampleRadius = Math.ceil(radius);
  const weights = new Array(sampleRadius * 2 + 1);
  let totalWeight = 0;
  
  for (let i = -sampleRadius; i <= sampleRadius; i++) {
    const distance = Math.abs(i);
    let weight;
    
    if (distance <= radius) {
      // Linear falloff within radius
      weight = 1.0 - (distance / radius);
    } else {
      weight = 0;
    }
    
    weights[i + sampleRadius] = weight;
    totalWeight += weight;
  }
  
  // Normalize weights
  for (let i = 0; i < weights.length; i++) {
    weights[i] /= totalWeight;
  }
  
  return { weights, sampleRadius };
}

// Buffer cleanup (from graph.mjs)
function cleanupBlurBuffers() {
  console.log(`🧹 Cleaning up blur buffers`);
  blurBuffer1 = null;
  blurBuffer2 = null;
  blurCache.clear();
  
  // Force garbage collection if available
  if (global.gc) {
    global.gc();
    console.log(`♻️ Forced garbage collection`);
  }
}

// Manual blur implementation (simplified from graph.mjs)
function manualBlur(radius, logMemory = true) {
  const blurStart = performance.now();
  blurFrameCount++;
  
  // Memory tracking
  const memBefore = process.memoryUsage();
  if (logMemory) {
    console.log(`🔍 BLUR START Frame ${blurFrameCount}: RSS: ${(memBefore.rss / 1024 / 1024).toFixed(1)}MB, Heap: ${(memBefore.heapUsed / 1024 / 1024).toFixed(1)}MB`);
  }
  
  // Aggressive cleanup every 5 frames
  if (blurFrameCount % 5 === 0) {
    console.log(`🧹 Aggressive blur buffer cleanup at frame ${blurFrameCount}`);
    cleanupBlurBuffers();
  }
  
  // Safety check for detached buffer
  if (pixels.buffer && pixels.buffer.detached) {
    console.warn('🚨 Pixels buffer detached, recreating');
    pixels = new Uint8ClampedArray(width * height * 4);
    pixels.fill(0);
  }
  
  // Buffer allocation
  const bufferSize = width * height * 4;
  if (!blurBuffer1 || blurBuffer1.length !== bufferSize) {
    console.log(`📦 Allocating blur buffers: ${(bufferSize / 1024 / 1024).toFixed(1)}MB each`);
    blurBuffer1 = new Uint8ClampedArray(bufferSize);
    blurBuffer2 = new Uint8ClampedArray(bufferSize);
  }
  
  // Copy source to working buffer
  blurBuffer1.set(pixels);
  
  // Get blur weights
  const { weights, sampleRadius } = generateBlurWeights(radius);
  
  // Horizontal blur pass
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      let r = 0, g = 0, b = 0, totalAlphaWeight = 0, maxAlpha = 0;
      
      for (let i = -sampleRadius; i <= sampleRadius; i++) {
        const sampleX = Math.max(0, Math.min(width - 1, x + i));
        const index = (y * width + sampleX) * 4;
        const weight = weights[i + sampleRadius];
        const alpha = blurBuffer1[index + 3];
        
        if (alpha > 0) {
          const alphaWeight = (alpha / 255.0) * weight;
          r += blurBuffer1[index] * alphaWeight;
          g += blurBuffer1[index + 1] * alphaWeight;
          b += blurBuffer1[index + 2] * alphaWeight;
          totalAlphaWeight += alphaWeight;
        }
        
        maxAlpha = Math.max(maxAlpha, alpha * weight);
      }
      
      const index = (y * width + x) * 4;
      if (totalAlphaWeight > 0) {
        blurBuffer2[index] = r / totalAlphaWeight;
        blurBuffer2[index + 1] = g / totalAlphaWeight;
        blurBuffer2[index + 2] = b / totalAlphaWeight;
        blurBuffer2[index + 3] = Math.max(blurBuffer1[index + 3], maxAlpha * 0.9);
      } else {
        blurBuffer2[index] = blurBuffer1[index];
        blurBuffer2[index + 1] = blurBuffer1[index + 1];
        blurBuffer2[index + 2] = blurBuffer1[index + 2];
        blurBuffer2[index + 3] = blurBuffer1[index + 3];
      }
    }
    
    // Progress indicator for long operations
    if (y % 200 === 0 && y > 0) {
      const progress = (y / height * 100).toFixed(1);
      process.stdout.write(`\r🔄 Horizontal pass: ${progress}%`);
    }
  }
  console.log(''); // New line after progress
  
  // Vertical blur pass
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      let r = 0, g = 0, b = 0, totalAlphaWeight = 0, maxAlpha = 0;
      
      for (let i = -sampleRadius; i <= sampleRadius; i++) {
        const sampleY = Math.max(0, Math.min(height - 1, y + i));
        const index = (sampleY * width + x) * 4;
        const weight = weights[i + sampleRadius];
        const alpha = blurBuffer2[index + 3];
        
        if (alpha > 0) {
          const alphaWeight = (alpha / 255.0) * weight;
          r += blurBuffer2[index] * alphaWeight;
          g += blurBuffer2[index + 1] * alphaWeight;
          b += blurBuffer2[index + 2] * alphaWeight;
          totalAlphaWeight += alphaWeight;
        }
        
        maxAlpha = Math.max(maxAlpha, alpha * weight);
      }
      
      const index = (y * width + x) * 4;
      if (totalAlphaWeight > 0) {
        pixels[index] = r / totalAlphaWeight;
        pixels[index + 1] = g / totalAlphaWeight;
        pixels[index + 2] = b / totalAlphaWeight;
        pixels[index + 3] = Math.max(blurBuffer2[index + 3], maxAlpha * 0.9);
      } else {
        pixels[index] = blurBuffer2[index];
        pixels[index + 1] = blurBuffer2[index + 1];
        pixels[index + 2] = blurBuffer2[index + 2];
        pixels[index + 3] = blurBuffer2[index + 3];
      }
    }
    
    // Progress indicator
    if (y % 200 === 0 && y > 0) {
      const progress = (y / height * 100).toFixed(1);
      process.stdout.write(`\r🔄 Vertical pass: ${progress}%`);
    }
  }
  console.log(''); // New line after progress
  
  // Memory tracking after blur
  const memAfter = process.memoryUsage();
  const duration = performance.now() - blurStart;
  const rssDelta = (memAfter.rss - memBefore.rss) / 1024 / 1024;
  const heapDelta = (memAfter.heapUsed - memBefore.heapUsed) / 1024 / 1024;
  
  if (logMemory) {
    console.log(`🔍 BLUR END Frame ${blurFrameCount}: RSS: ${(memAfter.rss / 1024 / 1024).toFixed(1)}MB (${rssDelta >= 0 ? '+' : ''}${rssDelta.toFixed(1)}), Heap: ${(memAfter.heapUsed / 1024 / 1024).toFixed(1)}MB (${heapDelta >= 0 ? '+' : ''}${heapDelta.toFixed(1)})`);
    console.log(`⏱️ Blur duration: ${duration.toFixed(1)}ms`);
  }
  
  return {
    duration,
    memBefore,
    memAfter,
    rssDelta,
    heapDelta
  };
}

// Main stress test
async function runStressTest() {
  console.log(`🚀 Starting blur stress test`);
  console.log(`📐 Resolution: ${width}x${height} (${(width * height * 4 / 1024 / 1024).toFixed(1)}MB pixel buffer)`);
  console.log(`🎯 Target: 120 consecutive blur operations\n`);
  
  const results = [];
  const startTime = performance.now();
  let crashed = false;
  
  try {
    for (let frame = 1; frame <= 120; frame++) {
      console.log(`\n🎬 === FRAME ${frame}/120 ===`);
      
      // Generate new noise pattern
      generateNoiseBuffer(frame);
      
      // Run blur with detailed profiling
      const result = manualBlur(3, true);
      
      // Store results for analysis
      results.push({
        frame,
        duration: result.duration,
        rssMemory: result.memAfter.rss / 1024 / 1024,
        heapMemory: result.memAfter.heapUsed / 1024 / 1024,
        rssDelta: result.rssDelta,
        heapDelta: result.heapDelta
      });
      
      // Check for warning signs
      if (result.duration > 500) {
        console.log(`⚠️ WARNING: Slow blur detected (${result.duration.toFixed(1)}ms)`);
      }
      
      if (result.memAfter.rss / 1024 / 1024 > 200) {
        console.log(`⚠️ WARNING: High memory usage (${(result.memAfter.rss / 1024 / 1024).toFixed(1)}MB RSS)`);
      }
      
      // Simulate the timing of real recording
      if (frame % 10 === 0) {
        console.log(`📊 Progress: ${frame}/120 frames (${(frame/120*100).toFixed(1)}%)`);
      }
    }
    
    console.log(`\n✅ Stress test completed successfully!`);
    
  } catch (error) {
    crashed = true;
    console.error(`\n💥 CRASH at frame ${results.length + 1}: ${error.message}`);
    console.error(error.stack);
  }
  
  // Generate detailed report
  const totalTime = (performance.now() - startTime) / 1000;
  console.log(`\n📊 === PERFORMANCE ANALYSIS ===`);
  console.log(`Total test duration: ${totalTime.toFixed(1)}s`);
  console.log(`Frames processed: ${results.length}/120`);
  console.log(`Status: ${crashed ? '💥 CRASHED' : '✅ SUCCESS'}`);
  
  if (results.length > 0) {
    const avgDuration = results.reduce((sum, r) => sum + r.duration, 0) / results.length;
    const maxDuration = Math.max(...results.map(r => r.duration));
    const minDuration = Math.min(...results.map(r => r.duration));
    
    const finalMemory = results[results.length - 1];
    const startMemory = results[0];
    
    console.log(`\nBlur performance:`);
    console.log(`  Average: ${avgDuration.toFixed(1)}ms`);
    console.log(`  Min: ${minDuration.toFixed(1)}ms`);
    console.log(`  Max: ${maxDuration.toFixed(1)}ms`);
    
    console.log(`\nMemory usage:`);
    console.log(`  Start RSS: ${startMemory.rssMemory.toFixed(1)}MB`);
    console.log(`  Final RSS: ${finalMemory.rssMemory.toFixed(1)}MB`);
    console.log(`  Net change: ${(finalMemory.rssMemory - startMemory.rssMemory).toFixed(1)}MB`);
    console.log(`  Peak RSS: ${Math.max(...results.map(r => r.rssMemory)).toFixed(1)}MB`);
  }
  
  // Save detailed results
  const reportPath = '/workspaces/aesthetic-computer/reference/tools/output/blur-stress-test-report.json';
  const report = {
    timestamp: new Date().toISOString(),
    resolution: { width, height },
    crashed,
    framesProcessed: results.length,
    totalDuration: totalTime,
    results
  };
  
  writeFileSync(reportPath, JSON.stringify(report, null, 2));
  console.log(`\n📋 Detailed report saved: ${reportPath}`);
}

// Run the test
runStressTest().catch(console.error);