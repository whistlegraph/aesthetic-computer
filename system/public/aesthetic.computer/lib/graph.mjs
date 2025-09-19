import {
  p2,
  number,
  wrap,
  randInt,
  byteInterval17,
  even,
  radians,
  lerp,
  randIntRange,
  randIntArr,
  clamp,
  signedCeil,
  rainbow,
  zebra,
  resetRainbowCache as numResetRainbowCache,
  resetZebraCache as numResetZebraCache,
  isHexString,
  hexToRgb,
  shiftRGB,
  cssColors,
  parseColorIndex,
  rand,
} from "./num.mjs";

import * as mat4 from "../dep/gl-matrix/mat4.mjs";
import * as vec2 from "../dep/gl-matrix/vec2.mjs";
import * as vec3 from "../dep/gl-matrix/vec3.mjs";
import * as vec4 from "../dep/gl-matrix/vec4.mjs";

import { any, repeat, nonvalue, flip } from "./help.mjs";
import { Box } from "./geo.mjs";
import { nanoid } from "../dep/nanoid/nanoid.js";

const { round, sign, abs, ceil, floor, sin, cos, min, max, sqrt, PI } = Math;

let width, height, pixels;
const depthBuffer = [];
const writeBuffer = [];
const c = [255, 255, 255, 255];
let c2 = null; // Alternate / secondary color support.
const panTranslation = { x: 0, y: 0 }; // For 2d shifting using `pan` and `unpan`.
let activeMask; // A box for totally masking the renderer.
//                 This should work everywhere.
const skips = [];

// Legacy global fade variables (kept for backward compatibility)
// Note: New code should use local fade system via parseLocalFade/getLocalFadeColor
let fadeMode = false;
let fadeColors = [];
let fadeDirection = "horizontal";
let fadeNeat = false;

// 🚨 GRAPH PERFORMANCE TRACKING
const graphPerf = {
  enabled: false,
  functions: new Map(),
  lastFPS: 0, // Track current FPS for frame skipping
  track(name, duration) {
    if (!this.enabled) return;
    if (!this.functions.has(name)) {
      this.functions.set(name, { count: 0, totalTime: 0, maxTime: 0 });
    }
    const stats = this.functions.get(name);
    stats.count++;
    stats.totalTime += duration;
    stats.maxTime = Math.max(stats.maxTime, duration);
    
    // Warn about slow functions (reduced threshold for important debugging)
    if (duration > 15) {
      console.warn(`🐌 SLOW GRAPH FUNCTION: ${name} took ${duration.toFixed(2)}ms`);
    }
  },
  reset() {
    this.functions.clear();
  },
  getStats() {
    const stats = [];
    this.functions.forEach((data, name) => {
      stats.push({
        name,
        count: data.count,
        totalTime: data.totalTime,
        avgTime: data.totalTime / data.count,
        maxTime: data.maxTime
      });
    });
    return stats.sort((a, b) => b.totalTime - a.totalTime);
  }
};

// Make graphPerf globally accessible for performance monitoring
if (typeof window !== 'undefined') {
  window.graphPerf = graphPerf;
}

// Also try globalThis for broader compatibility
if (typeof globalThis !== 'undefined') {
  globalThis.graphPerf = graphPerf;
}

// 📚 DOCUMENTATION: Fade support - "fade:color1-color2-color3" syntax
// Fade state - used by ink() function for smooth color transitions
// Removed global fade state - everything is now local!
let currentKidLispContext = null; // For dynamic evaluation of fade directions
let currentRainbowColor = null; // Cache rainbow color for current drawing operation
let currentZebraColor = null; // Cache zebra color for current drawing operation

let debug = false;
export function setDebug(newDebug) {
  debug = newDebug;
}

// 🎨 FADE HELPERS: New local fade detection and parsing
// These replace the global fade state management system

/**
 * Check if a color contains a fade string
 * @param {Array} color - Color array that might contain fade string
 * @returns {boolean} - True if color contains fade string
 */
function isFadeColor(color) {
  return Array.isArray(color) && 
         color.length > 0 && 
         typeof color[0] === 'string' && 
         color[0].startsWith('fade:');
}

/**
 * Parse fade color locally without setting global state
 * @param {Array} color - Color array containing fade string
 * @returns {Object|null} - Fade info object or null if not a fade
 */
/**
 * Parse fade colors locally without setting any global state
 * @param {string} fadeType - The fade type (e.g., "red-blue", "rainbow-black-rainbow")
 * @param {number} alpha - Alpha value to apply to all colors
 * @returns {Array} - Array of color arrays
 */
function parseLocalFade(fadeType, alpha = 255) {
  // Handle color combinations separated by dashes
  const colorNames = fadeType.split("-");
  const colors = [];
  
  // For symmetric patterns, we want the same sequence colors to have the same base
  // but still support consecutive different colors like rainbow-rainbow-rainbow
  let rainbowBaseOffset = 0;
  let zebraBaseOffset = 0;
  
  for (let i = 0; i < colorNames.length; i++) {
    const colorName = colorNames[i];
    
    if (colorName === "rainbow") {
      // Check if this is a consecutive rainbow (previous color was also rainbow)
      const consecutiveOffset = (i > 0 && colorNames[i-1] === "rainbow") ? 1 : 0;
      colors.push(["rainbow", 255, 255, 255, alpha, rainbowBaseOffset + consecutiveOffset]);
      if (consecutiveOffset > 0) rainbowBaseOffset++;
    } else if (colorName === "zebra") {
      // Check if this is a consecutive zebra (previous color was also zebra)
      const consecutiveOffset = (i > 0 && colorNames[i-1] === "zebra") ? 1 : 0;
      colors.push(["zebra", 255, 255, 255, alpha, zebraBaseOffset + consecutiveOffset]);
      if (consecutiveOffset > 0) zebraBaseOffset++;
    } else if (colorName.startsWith("c")) {
      // Color index like c0, c1, etc.
      const indexColor = parseColorIndex(colorName);
      if (indexColor) {
        colors.push([...indexColor.slice(0, 3), alpha]);
      }
    } else {
      // Named CSS color
      const color = cssColors[colorName];
      if (color) {
        colors.push([...color.slice(0, 3), alpha]);
      } else {
        // If color not found, use a default color
        colors.push([128, 128, 128, alpha]);
      }
    }
  }
  
  return colors.length >= 2 ? colors : null;
}

function parseFadeColor(color) {
  if (!isFadeColor(color)) return null;
  
  const fadeString = color[0];
  const alpha = color[1] || 255; // Second element is alpha if provided
  
  // Parse fade string locally without setting global state
  const parts = fadeString.split(":");
  if (parts.length < 2 || parts.length > 4 || parts[0] !== "fade") {
    return null;
  }
  
  // Extract direction and neat flag locally
  let isNeat = false;
  let fadeType = parts[1];
  let direction = parts[2] || "horizontal";
  
  // Handle different neat syntax positions
  if (parts[1] === "neat" && parts[2]) {
    isNeat = true;
    fadeType = parts[2];
    direction = parts[3] || "horizontal";
  } else if (parts[2] === "neat") {
    isNeat = true;
    direction = "horizontal";
  } else if (parts[3] === "neat") {
    isNeat = true;
    direction = parts[2];
  } else if (parts.length > 2 && (parts[2] === "neat" || parts[parts.length - 1] === "neat")) {
    isNeat = true;
    const filteredParts = parts.filter(p => p !== "neat");
    if (filteredParts.length >= 2) {
      fadeType = filteredParts[1];
      direction = filteredParts[2] || "horizontal";
    }
  }
  
  // Parse fade colors using the local fade type and alpha
  const fadeColors = parseLocalFade(fadeType, alpha);
  if (!fadeColors) return null;
  
  return {
    colors: fadeColors,
    direction: direction,  // Local direction, no global state
    neat: isNeat          // Local neat flag, no global state
  };
}

/**
 * Get interpolated fade color locally without using global state
 * @param {number} t - Position along fade (0-1)
 * @param {number} x - X coordinate for noise (optional)
 * @param {number} y - Y coordinate for noise (optional)
 * @param {Object} fadeInfo - Local fade info from parseFadeColor
 * @returns {Array} - RGBA color array
 */
function getLocalFadeColor(t, x = 0, y = 0, fadeInfo) {
  if (!fadeInfo || fadeInfo.colors.length < 2) {
    return c.slice(); // Return current color if no fade info
  }
  
  const colors = fadeInfo.colors;
  
  // Clamp t to 0-1 range
  t = Math.max(0, Math.min(1, t));
  
  // Calculate which colors to interpolate between
  const segments = colors.length - 1;
  const segment = Math.min(Math.floor(t * segments), segments - 1);
  const localT = (t * segments) - segment;
  
  const color1 = colors[segment];
  const color2 = colors[segment + 1] || colors[segment]; // Fallback to same color if out of bounds
  
  // Safety check to prevent undefined access
  if (!color1 || !color2) {
    console.warn("🚨 getLocalFadeColor: Invalid color data", { segment, colors, t });
    return c.slice(); // Return current color as fallback
  }
  
  // Handle special dynamic colors (zebra first, then rainbow)
  if (color1[0] === "zebra" || color2[0] === "zebra") {
    // Don't use cached zebra color - always get fresh colors with offsets
    // This fixes the cyan zebra issue when zebra is first in the chain
    
    // Properly interpolate between zebra and regular colors
    if (color1[0] === "zebra" && color2[0] === "zebra") {
      // Both are zebra, get colors with their respective offsets
      const offset1 = color1[5] || 0; // Get offset from 6th element
      const offset2 = color2[5] || 0;
      const zebra1 = zebra(offset1);
      const zebra2 = zebra(offset2);
      
      const alpha1 = color1[4] || 255;
      const alpha2 = color2[4] || 255;
      const alpha = Math.round(alpha1 + (alpha2 - alpha1) * localT);
      
      return [
        Math.round(zebra1[0] + (zebra2[0] - zebra1[0]) * localT),
        Math.round(zebra1[1] + (zebra2[1] - zebra1[1]) * localT),
        Math.round(zebra1[2] + (zebra2[2] - zebra1[2]) * localT),
        alpha
      ];
    } else if (color1[0] === "zebra") {
      // Interpolate from zebra to regular/rainbow color
      const offset1 = color1[5] || 0;
      const zebra1 = zebra(offset1);
      const alpha1 = color1[4] || 255;
      
      // Handle if color2 is also a special color
      let color2RGB;
      let alpha2;
      if (color2[0] === "rainbow") {
        const offset2 = color2[5] || 0;
        color2RGB = rainbow(offset2);
        alpha2 = color2[4] || 255;
      } else {
        color2RGB = [color2[0], color2[1], color2[2]];
        alpha2 = color2[3] || 255;
      }
      
      return [
        Math.round(zebra1[0] + (color2RGB[0] - zebra1[0]) * localT),
        Math.round(zebra1[1] + (color2RGB[1] - zebra1[1]) * localT),
        Math.round(zebra1[2] + (color2RGB[2] - zebra1[2]) * localT),
        Math.round(alpha1 + (alpha2 - alpha1) * localT)
      ];
    } else {
      // Interpolate from regular color to zebra
      const offset2 = color2[5] || 0;
      const zebra2 = zebra(offset2);
      const alpha1 = color1[3] || 255;
      const alpha2 = color2[4] || 255;
      return [
        Math.round(color1[0] + (zebra2[0] - color1[0]) * localT),
        Math.round(color1[1] + (zebra2[1] - color1[1]) * localT),
        Math.round(color1[2] + (zebra2[2] - color1[2]) * localT),
        Math.round(alpha1 + (alpha2 - alpha1) * localT)
      ];
    }
  }
  
  if (color1[0] === "rainbow" || color2[0] === "rainbow") {
    // Don't use cached rainbow color - always get fresh colors with offsets
    // This keeps rainbow consistent with zebra behavior
    
    // Properly interpolate between rainbow and regular colors
    if (color1[0] === "rainbow" && color2[0] === "rainbow") {
      // Both are rainbow, get colors with their respective offsets
      const offset1 = color1[5] || 0; // Get offset from 6th element
      const offset2 = color2[5] || 0;
      const rainbow1 = rainbow(offset1);
      const rainbow2 = rainbow(offset2);
      
      const alpha1 = color1[4] || 255;
      const alpha2 = color2[4] || 255;
      const alpha = Math.round(alpha1 + (alpha2 - alpha1) * localT);
      
      return [
        Math.round(rainbow1[0] + (rainbow2[0] - rainbow1[0]) * localT),
        Math.round(rainbow1[1] + (rainbow2[1] - rainbow1[1]) * localT),
        Math.round(rainbow1[2] + (rainbow2[2] - rainbow1[2]) * localT),
        alpha
      ];
    } else if (color1[0] === "rainbow") {
      // Interpolate from rainbow to regular/zebra color
      const offset1 = color1[5] || 0;
      const rainbow1 = rainbow(offset1);
      const alpha1 = color1[4] || 255;
      
      // Handle if color2 is also a special color
      let color2RGB;
      let alpha2;
      if (color2[0] === "zebra") {
        const offset2 = color2[5] || 0;
        color2RGB = zebra(offset2);
        alpha2 = color2[4] || 255;
      } else {
        color2RGB = [color2[0], color2[1], color2[2]];
        alpha2 = color2[3] || 255;
      }
      
      return [
        Math.round(rainbow1[0] + (color2RGB[0] - rainbow1[0]) * localT),
        Math.round(rainbow1[1] + (color2RGB[1] - rainbow1[1]) * localT),
        Math.round(rainbow1[2] + (color2RGB[2] - rainbow1[2]) * localT),
        Math.round(alpha1 + (alpha2 - alpha1) * localT)
      ];
    } else {
      // Interpolate from regular color to rainbow
      const offset2 = color2[5] || 0;
      const rainbow2 = rainbow(offset2);
      const alpha1 = color1[3] || 255;
      const alpha2 = color2[4] || 255;
      return [
        Math.round(color1[0] + (rainbow2[0] - color1[0]) * localT),
        Math.round(color1[1] + (rainbow2[1] - color1[1]) * localT),
        Math.round(color1[2] + (rainbow2[2] - color1[2]) * localT),
        Math.round(alpha1 + (alpha2 - alpha1) * localT)
      ];
    }
  }
  
  if (color1[0] === "zebra" || color2[0] === "zebra") {
    console.log("🦓 Taking zebra branch");
    // Don't use cached zebra color - always get fresh colors with offsets
    // This fixes the cyan zebra issue when zebra is first in the chain
    
    // Properly interpolate between zebra and regular colors
    if (color1[0] === "zebra" && color2[0] === "zebra") {
      // Both are zebra, get colors with their respective offsets
      const offset1 = color1[5] || 0; // Get offset from 6th element
      const offset2 = color2[5] || 0;
      const zebra1 = zebra(offset1);
      const zebra2 = zebra(offset2);
      
      const alpha1 = color1[4] || 255;
      const alpha2 = color2[4] || 255;
      const alpha = Math.round(alpha1 + (alpha2 - alpha1) * localT);
      
      return [
        Math.round(zebra1[0] + (zebra2[0] - zebra1[0]) * localT),
        Math.round(zebra1[1] + (zebra2[1] - zebra1[1]) * localT),
        Math.round(zebra1[2] + (zebra2[2] - zebra1[2]) * localT),
        alpha
      ];
    } else if (color1[0] === "zebra") {
      // Interpolate from zebra to regular/rainbow color
      const offset1 = color1[5] || 0;
      const zebra1 = zebra(offset1);
      
      // DEBUG: Log zebra color when it's first
      console.log("🦓 DEBUG zebra first:", { offset1, zebra1, color1 });
      
      const alpha1 = color1[4] || 255;
      
      // Handle if color2 is also a special color
      let color2RGB;
      let alpha2;
      if (color2[0] === "rainbow") {
        const offset2 = color2[5] || 0;
        color2RGB = rainbow(offset2);
        alpha2 = color2[4] || 255;
      } else {
        color2RGB = [color2[0], color2[1], color2[2]];
        alpha2 = color2[3] || 255;
      }
      
      return [
        Math.round(zebra1[0] + (color2RGB[0] - zebra1[0]) * localT),
        Math.round(zebra1[1] + (color2RGB[1] - zebra1[1]) * localT),
        Math.round(zebra1[2] + (color2RGB[2] - zebra1[2]) * localT),
        Math.round(alpha1 + (alpha2 - alpha1) * localT)
      ];
    } else {
      // Interpolate from regular color to zebra
      const offset2 = color2[5] || 0;
      const zebra2 = zebra(offset2);
      const alpha1 = color1[3] || 255;
      const alpha2 = color2[4] || 255;
      return [
        Math.round(color1[0] + (zebra2[0] - color1[0]) * localT),
        Math.round(color1[1] + (zebra2[1] - color1[1]) * localT),
        Math.round(color1[2] + (zebra2[2] - color1[2]) * localT),
        Math.round(alpha1 + (alpha2 - alpha1) * localT)
      ];
    }
  }
  
  // Linear interpolation between regular colors
  const r = Math.round(color1[0] + (color2[0] - color1[0]) * localT);
  const g = Math.round(color1[1] + (color2[1] - color1[1]) * localT);
  const b = Math.round(color1[2] + (color2[2] - color1[2]) * localT);
  const a = Math.round(color1[3] + (color2[3] - color1[3]) * localT);
  
  // Add noise if not neat mode (same logic as getFadeColor)
  if (!fadeInfo.neat) {
    const noiseAmount = 8;
    const noiseR = (Math.random() - 0.5) * noiseAmount;
    const noiseG = (Math.random() - 0.5) * noiseAmount;
    const noiseB = (Math.random() - 0.5) * noiseAmount;
    
    return [
      Math.max(0, Math.min(255, r + noiseR)),
      Math.max(0, Math.min(255, g + noiseG)),
      Math.max(0, Math.min(255, b + noiseB)),
      a
    ];
  }
  
  return [r, g, b, a];
}

// 🔧 DEBUG: Helper function to get current global dimensions
function getGlobalDimensions() {
  return `${width}x${height}`;
}

// 🔧 DEBUG: Export for debugging buffer context issues
export { getGlobalDimensions };

let twoDCommands;
export function twoD(ref) {
  twoDCommands = ref;
}

// 1. Configuration & State
function makeBuffer(width, height, fillProcess, painting, api) {
  if (!width || !height) return;

  const bufferStart = performance.now(); // 🔍 PERFORMANCE DEBUG

  const imageData = new ImageData(width, height);

  const buffer = {
    pixels: imageData.data,
    width: imageData.width,
    height: imageData.height,
  };

  buffer.api = api;

  if (typeof fillProcess === "function") {
    // Remember the current buffer and color.
    const savedBuffer = getBuffer();
    const rc = c; // Remember color.
    setBuffer(buffer);
    api.screen.pixels = buffer.pixels; // Set the API's pixel buffer.
    
    // 🚀 OPTIMIZATION: Fast path for simple operations
    try {
      fillProcess(api); // Every fill process gets a painting API.
      // Only call paint if there's actually something to paint
      if (painting && typeof painting.paint === 'function') {
        painting.paint(true);
      }
    } catch (error) {
      console.warn('⚠️ makeBuffer fillProcess error:', error);
    }
    
    // Restore old buffer and color.
    setBuffer(savedBuffer);
    color(...rc);
  }

  const bufferTime = performance.now() - bufferStart; // 🔍 PERFORMANCE DEBUG
  // Disabled buffer creation logging for cleaner console
  // if (bufferTime > 10) { // Only log very slow buffer creation
  //   console.log(`🔍 graph.makeBuffer took ${bufferTime.toFixed(2)}ms (${width}x${height})`);
  // }

  return buffer;
}

// Returns a cloned pixel buffer.
function cloneBuffer(buffer) {
  return {
    width: buffer.width,
    height: buffer.height,
    pixels: new Uint8ClampedArray(buffer.pixels),
  };
}

function getBuffer() {
  return { width, height, pixels };
}

function setBuffer(buffer) {
  // Check if the buffer's pixels array is detached and recreate if necessary
  if (buffer.pixels && buffer.pixels.buffer && buffer.pixels.buffer.detached) {
    console.warn('🚨 Detected detached pixels buffer in setBuffer, recreating... (This should be rare now)');
    // Recreate the buffer with the same dimensions, filled with transparent pixels
    buffer.pixels = new Uint8ClampedArray(buffer.width * buffer.height * 4);
    buffer.pixels.fill(0); // Fill with transparent black
  }
  
  ({ width, height, pixels } = buffer);
}

function changePixels(changer) {
  changer(pixels, width, height);
}

// Return a pixel from the main buffer or from a specified buffer.
function pixel(x, y, painting = { width, height, pixels }) {
  const buffer = painting.pixels;
  if (x >= painting.width || y >= painting.height || x < 0 || y < 0) {
    return [0, 0, 0, 0];
  }
  const i = (floor(x) + floor(y) * painting.width) * 4;
  return [buffer[i], buffer[i + 1], buffer[i + 2], buffer[i + 3]];
}

// Helper function to check if two colors are the same
function colorsMatch(color1, color2) {
  if (!color1) return false;
  return (
    color1[0] === color2[0] &&
    color1[1] === color2[1] &&
    color1[2] === color2[2] &&
    color1[3] === color2[3]
  );
}

// Fill pixels with a color using a flood fill technique.
function flood(x, y, fillColor = c) {
  // Note: Fade flood fill temporarily disabled during global fade system removal
  // TODO: Implement local fade flood fill later
  
  // Get the target color of the pixel at (x, y)
  const targetColor = pixel(x, y);
  if (targetColor[3] === 0) {
    // If the target pixel is transparent, return
    return {
      color: targetColor,
      area: 0,
    };
  }

  let count = 0;
  const visited = new Set();
  const stack = [[x, y]];

  color(...findColor(fillColor));
  const oldColor = c;
  while (stack.length) {
    const [cx, cy] = stack.pop();
    const key = `${cx},${cy}`;

    if (visited.has(key)) continue;
    visited.add(key);

    const currentColor = pixel(cx, cy);
    if (colorsMatch(currentColor, targetColor)) {
      count++;

      plot(cx, cy);

      stack.push([cx + 1, cy]); // Push neighbors to stack.
      stack.push([cx - 1, cy]);
      stack.push([cx, cy + 1]);
      stack.push([cx, cy - 1]);
    }
  }

  color(...oldColor);

  return {
    color: targetColor,
    area: count,
  };
}

// Gradient flood fill for fade colors
// gradientFlood function temporarily removed during global fade system refactor
// TODO: Implement local fade flood fill using the new local fade system

// getFadeColor function removed - everything uses local getLocalFadeColor now!

// Reset rainbow and zebra cache for new drawing operations
function resetRainbowCache() {
  currentRainbowColor = null;
  currentZebraColor = null;
  // Also reset the num.mjs frame caches
  numResetRainbowCache();
  numResetZebraCache();
}

// Legacy getFadeColor function (for backward compatibility)
// New code should use getLocalFadeColor instead
function getFadeColor(t, x = 0, y = 0) {
  if (!fadeMode || fadeColors.length < 2) {
    return c.slice(); // Return current color if no fade
  }
  
  // For backward compatibility, create local fade info and use local function
  const fadeInfo = {
    colors: fadeColors,
    direction: fadeDirection,
    neat: fadeNeat
  };
  
  return getLocalFadeColor(t, x, y, fadeInfo);
}

// Parse a color from a variety of inputs..
function findColor() {
  let args = [...arguments];

  if (args.length === 1 && args[0] !== undefined) {
    const isNumber = () => typeof args[0] === "number";
    const isArray = () => Array.isArray(args[0]);
    const isString = () => typeof args[0] === "string";
    const isFadeObject = () => typeof args[0] === "object" && args[0] !== null && args[0].type === "fade";
    const isBool = typeof args[0] === "boolean";

    // Handle fade objects (from parseColor) - now completely local
    if (isFadeObject()) {
      const fadeObj = args[0];
      // Extract alpha value from array if needed (for range syntax like "32-64")
      const alphaValue = Array.isArray(fadeObj.alpha) ? fadeObj.alpha[0] : fadeObj.alpha;
      // Create fade color array format for local detection
      const fadeColorArray = [fadeObj.fadeString, alphaValue || 255];
      return fadeColorArray; // Return as is for local processing
    }

    if (isBool) {
      // No more global fade state to reset
      resetRainbowCache();
      return args[0] ? [255, 255, 255, 255] : [0, 0, 0, 255];
    }

    // If it's not a Number or Array or String or FadeObject, then assume it's an object,
    // randomly pick a key & re-run.
    if (!isNumber() && !isArray() && !isString() && !isFadeObject())
      return findColor(any(args[0]));

    // Single number argument.
    if (isNumber()) {
      // No more global fade state to reset
      resetRainbowCache();
      
      // Treat as raw hex if we hit a certain limit.
      if (args[0] > 255) {
        args = hexToRgb(args[0]);
      } else {
        // Otherwise, replicate the first number across all three fields.
        args = Array.from(args);
        args.push(args[0], args[0]);
      }
    } else if (isArray()) {
      // No more global fade state handling needed
      return findColor(...args[0]);
    } else if (isString()) {
      // Check for fade syntax first (for direct fade strings)
      if (args[0].startsWith("fade:")) {
        // Return as fade color array for local processing
        return [args[0], 255]; // Default alpha for direct strings
      }
      
      // No more global fade state to reset
      resetRainbowCache();
      
      // FIRST: Check for color index format like "c0", "c1", "p0", etc.
      const indexColor = parseColorIndex(args[0]);
      if (indexColor) {
        if (indexColor[0] === "rainbow") {
          args = rainbow(); // Handle p0 rainbow palette
        } else {
          args = indexColor; // Handle c0, c1, etc.
        }
      } else if (args[0] === "erase") {
        // TODO: Add better alpha support here... 23.09.11.22.10
        //       ^ See `parseColor` in `num`.
        // let alpha = 255;
        // if (args[1]) alpha = parseFloat(args[1]);
        args = [-1, -1, -1];
        if (args[1]) args.push(computeAlpha(args[1]));
      } else if (args[0] === "rainbow") {
        args = rainbow(); // Cycle through roygbiv in a linear sequence.
      } else {
        // See if it's a hex.
        const cleanedHex = args[0]
          .replace("#", "")
          .replace("0x", "")
          .toUpperCase();
        if (isHexString(cleanedHex) === true) {
          args = hexToRgb(cleanedHex);
        } else if (args[0].startsWith("fade:")) {
          // Fade operations should have been handled above - this is a fallback
          args = [255, 0, 255]; // Fallback to magenta to indicate error
          args.push(255);
        } else {
          // Try CSS color table lookup
          const cssColor = cssColors[args[0]];
          if (cssColor) {
            args = cssColor;
          } else {
            // Fallback to random color
            args = randIntArr(255, 3);
            args.push(255);
          }
        }
      }
      // TODO: Add an error message here. 22.08.29.13.03
    }
  } else if (args.length === 2) {
    // rainbow, alpha
    if (args[0] === "rainbow") {
      args = [...rainbow(), computeAlpha(args[1])];
    } else if (typeof args[0] === "string") {
      // Check for fade syntax with alpha
      if (args[0].startsWith("fade:")) {
        // Return local fade info instead of setting global state
        const fadeString = args[0];
        const alpha = computeAlpha(args[1] || 255);
        return [fadeString, alpha]; // Return as fade color array for local processing
      }
      
      // Check for color index format with alpha
      const indexColor = parseColorIndex(args[0]);
      if (indexColor) {
        if (indexColor[0] === "rainbow") {
          args = [...rainbow(), computeAlpha(args[1])];
        } else {
          args = [...indexColor, computeAlpha(args[1])];
        }
      } else {
        // Try CSS color table lookup
        const cssColor = cssColors[args[0]];
        if (cssColor) {
          args = [...cssColor, computeAlpha(args[1])];
        } else {
          args = [0, 0, 0, computeAlpha(args[1])]; // Fallback to black
        }
      }
    } else if (Array.isArray(args[0])) {
      args = [...args[0], args[1]];
    } else if (args[0] === undefined) {
      // Random color with specified alpha: (undefined, alpha)
      const alphaValue = args[1]; // Store the alpha before generating random RGB
      args = randIntArr(255, 3);
      args.push(computeAlpha(alphaValue)); // Use specified alpha
    } else {
      // rgb, a
      args = [args[0], args[0], args[0], args[1]];
    }
  } else if (
    args.length === 0 ||
    (args.length === 1 && args[0] === undefined)
  ) {
    args = randIntArr(255, 3);
    args.push(255); // Generate random values here, always leave alpha 255.
  }

  if (args.length === 3) args = [...args, 255]; // Always be sure we have alpha.

  // Debug: Check for problematic color values and filter out timing expressions
  // Exclude valid fade strings from the invalid check
  if (args.some(val => val === undefined || 
                      (isNaN(val) && !(typeof val === 'string' && val.startsWith('fade:'))) || 
                      (typeof val === 'string' && val.match(/^\d*\.?\d+s\.\.\.?$/)))) {
    console.log("WARNING: Invalid color values detected:", args);
    
    // Check if this contains a fade string that got passed incorrectly
    const fadeString = args.find(val => typeof val === 'string' && val.startsWith('fade:'));
    if (fadeString) {
      // Return fade color array format
      const alpha = args.find(val => typeof val === 'number' && !isNaN(val)) || 255;
      return [fadeString, alpha];
    }
    
    // Filter out timing expressions that were incorrectly passed as color arguments
    args = args.filter(val => !(typeof val === 'string' && val.match(/^\d*\.?\d+s\.\.\.?$/)));
    
    // If we filtered out timing expressions, we might be short on arguments
    while (args.length < 3) {
      args.push(255); // Add default RGB values
    }
    if (args.length === 3) args.push(255); // Add alpha if missing
    
    // Clean up any remaining invalid values
    args = args.map((val, i) => {
      if (val === undefined || isNaN(val) || typeof val === 'string') {
        return i === 3 ? 255 : randInt(255); // Alpha defaults to 255, RGB gets random
      }
      return val;
    });
    
    // Ensure we have exactly 4 values
    args = args.slice(0, 4);
    while (args.length < 4) {
      args.push(args.length === 3 ? 255 : randInt(255));
    }
  }

  // Randomized any undefined or null values across all 4 arguments.
  args.forEach((a, i) => {
    if (isNaN(args[i])) args[i] = randInt(255);
  });

  return args;
}

// TODO: How could I convert from 0->1 and 0->255 with no side effects?
// TODO: What's a good way for ink to accept different ranges of alpha?
//       24.08.20.20.12
// Current solution:
// Any number from 0-><1 will use 0-> alpha.
//  - 1 and above will use 0->255.
//  - 0 always bottoms out
// Edge case near the 1 is manageable.
function computeAlpha(alpha) {
  if (alpha > 0 && alpha < 1) alpha = round(alpha * 255);
  return alpha;
}

// Set global color.
// Internal function to set color without resetting fade mode
function setColor(r, g, b, a = 255) {
  c[0] = floor(r);
  c[1] = floor(g);
  c[2] = floor(b);
  c[3] = floor(a);
  return c.slice();
}

// Send 0 arguements to retrieve the current one.
function color(r, g, b, a = 255) {
  if (arguments.length === 0) return c.slice();
  
  // Check if this is a fade color string (when called as color("fade:red-lime", 255))
  if (typeof r === 'string' && r.startsWith('fade:')) {
    // Store the fade color array directly in c for box() to detect
    c[0] = r; // fade string
    c[1] = g || 255; // alpha from second argument
    c[2] = 0; // unused for fade
    c[3] = 255; // default alpha fallback
    return c.slice();
  }
  
  // Check if this is a fade color array format (when called as color(["fade:red-lime", 255]))
  if (arguments.length === 1 && Array.isArray(r) && typeof r[0] === 'string' && r[0].startsWith('fade:')) {
    // Store the fade color array directly in c for box() to detect
    c[0] = r[0]; // fade string
    c[1] = r[1] || 255; // alpha
    c[2] = 0; // unused for fade
    c[3] = 255; // default alpha fallback
    return c.slice();
  }
  
  // No more global fade state to manage - everything is local now!
  
  return setColor(r, g, b, a);
}

// Support for secondary color with the ability to clear the color by
// passing null or undefined as the first parameter.
function color2(r, g, b, a = 255) {
  if (arguments.length === 0) return c2.slice();
  if (r === undefined || r === null) {
    c2 = null;
    return;
  }
  if (!c2) c2 = [];
  c2[0] = floor(r);
  c2[1] = floor(g);
  c2[2] = floor(b);
  c2[3] = floor(a);
  return c2.slice();
}

export {
  makeBuffer,
  cloneBuffer,
  setBuffer,
  getBuffer,
  changePixels,
  depthBuffer,
  writeBuffer,
  color,
  color2,
  findColor,
  c, // currentColor
  pixel,
};

// Helper function to evaluate dynamic fade direction
function evaluateFadeDirection(directionStr) {
  // First try to parse as a number
  const numericAngle = parseFloat(directionStr);
  if (!isNaN(numericAngle)) {
    return numericAngle;
  }
  
  // Check for named directions
  const validDirections = [
    "horizontal", "horizontal-reverse", 
    "vertical", "vertical-reverse",
    "diagonal", "diagonal-reverse"
  ];
  if (validDirections.includes(directionStr)) {
    return directionStr;
  }
  
  // Check if it's a string representation of a number (e.g., "30")
  const trimmed = directionStr.trim();
  if (/^-?\d+(\.\d+)?$/.test(trimmed)) {
    const parsed = parseFloat(trimmed);
    if (!isNaN(parsed)) {
      return parsed;
    }
  }
  
  // Try to evaluate as KidLisp expression if we have context
  if (currentKidLispContext) {
    try {
      // Check if it's a JSON-encoded expression
      if (directionStr.startsWith('"') || directionStr.startsWith('[')) {
        const parsedExpression = JSON.parse(directionStr);
        const result = currentKidLispContext.evaluate(parsedExpression, currentKidLispContext.api, currentKidLispContext.env);
        const numResult = parseFloat(result);
        if (!isNaN(numResult)) {
          return numResult;
        }
      } else {
        // Try evaluating as a variable name
        const result = currentKidLispContext.evaluate(directionStr, currentKidLispContext.api, currentKidLispContext.env);
        const numResult = parseFloat(result);
        if (!isNaN(numResult)) {
          return numResult;
        }
      }
    } catch (error) {
      console.warn("Failed to evaluate fade direction expression:", directionStr, error);
    }
  }
  
  // Fallback to horizontal
  console.warn("Invalid fade direction, falling back to horizontal:", directionStr);
  return "horizontal";
}

// Helper function to calculate fade position based on angle (0-360 degrees)
function calculateAngleFadePosition(x, y, minX, minY, maxX, maxY, angle) {
  // Normalize angle to 0-360 range
  const originalAngle = angle;
  angle = ((angle % 360) + 360) % 360;
  
  // Get area dimensions
  const areaWidth = maxX - minX;
  const areaHeight = maxY - minY;
  
  // Calculate position relative to center of the area
  const centerX = minX + areaWidth / 2;
  const centerY = minY + areaHeight / 2;
  const relX = (x - centerX) / (areaWidth / 2);  // Normalize to -1 to 1
  const relY = (y - centerY) / (areaHeight / 2); // Normalize to -1 to 1
  
  // Convert angle to radians and calculate direction vector
  const radians = (angle * Math.PI) / 180;
  const dirX = Math.cos(radians);
  const dirY = -Math.sin(radians); // Negative because screen Y increases downward
  
  // Project current position onto the direction vector
  const dotProduct = relX * dirX + relY * dirY;
  
  // Normalize to 0-1 range (dotProduct ranges from -sqrt(2) to sqrt(2))
  const maxDot = Math.sqrt(2); // Maximum possible dot product
  const t = (dotProduct + maxDot) / (2 * maxDot);
  
  // Debug log only for first few pixels to avoid spam
  if (x < minX + 3 && y < minY + 3) {
    // Angle calculation debug log removed for performance
  }
  
  return Math.max(0, Math.min(1, t));
}

// 2. 2D Drawing
function clear() {
  // 🚀 OPTIMIZED CLEAR FUNCTION - Up to 10x faster!
  
  // Clean up blur buffers to prevent memory accumulation
  cleanupBlurBuffers();
  
  // Determine the area to clear (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = activeMask.x;
    minY = activeMask.y;
    maxX = activeMask.x + activeMask.width;
    maxY = activeMask.y + activeMask.height;
  }

  // 🎨 Check if current color is a fade string
  const fadeInfo = parseFadeColor(c);
  const isLocalFade = fadeInfo !== null;

  if (isLocalFade) {
    // Handle fade colors pixel by pixel (cannot use fast optimization)
    for (let y = minY; y < maxY; y++) {
      for (let x = minX; x < maxX; x++) {
        // Calculate fade position based on direction
        let t = 0;
        const numericAngle = parseFloat(fadeInfo.direction);
        
        if (!isNaN(numericAngle)) {
          // Use angle-based fade calculation
          t = calculateAngleFadePosition(x, y, minX, minY, maxX - 1, maxY - 1, numericAngle);
        } else {
          // Use named direction
          if (fadeInfo.direction === "horizontal") {
            t = (x - minX) / Math.max(1, (maxX - 1) - minX);
          } else if (fadeInfo.direction === "horizontal-reverse") {
            t = ((maxX - 1) - x) / Math.max(1, (maxX - 1) - minX);
          } else if (fadeInfo.direction === "vertical") {
            t = (y - minY) / Math.max(1, (maxY - 1) - minY);
          } else if (fadeInfo.direction === "vertical-reverse") {
            t = ((maxY - 1) - y) / Math.max(1, (maxY - 1) - minY);
          } else if (fadeInfo.direction === "diagonal") {
            const dx = (x - minX) / Math.max(1, (maxX - 1) - minX);
            const dy = (y - minY) / Math.max(1, (maxY - 1) - minY);
            t = (dx + dy) / 2;
          } else if (fadeInfo.direction === "diagonal-reverse") {
            const dx = ((maxX - 1) - x) / Math.max(1, (maxX - 1) - minX);
            const dy = ((maxY - 1) - y) / Math.max(1, (maxY - 1) - minY);
            t = (dx + dy) / 2;
          } else {
            // Default to horizontal
            t = (x - minX) / Math.max(1, (maxX - 1) - minX);
          }
        }
        
        // Get the fade color for this position
        const fadeColor = getLocalFadeColor(t, x, y, fadeInfo);
        
        // Set pixel directly
        const i = (x + y * width) * 4;
        if (i >= 0 && i < pixels.length - 3) {
          pixels[i] = fadeColor[0];     // r
          pixels[i + 1] = fadeColor[1]; // g
          pixels[i + 2] = fadeColor[2]; // b
          pixels[i + 3] = fadeColor[3]; // alpha
        }
      }
    }
  } else if (activeMask) {
    // Clear only the masked area with solid color (row by row optimization)
    const rowWidth = (maxX - minX) * 4;
    for (let y = minY; y < maxY; y++) {
      const rowStart = (y * width + minX) * 4;
      
      // Fill first pixel of row
      pixels[rowStart] = c[0];
      pixels[rowStart + 1] = c[1]; 
      pixels[rowStart + 2] = c[2];
      pixels[rowStart + 3] = c[3];
      
      // Use copyWithin to duplicate the pattern across the row (much faster!)
      if (rowWidth > 4) {
        for (let copySize = 4; copySize < rowWidth; copySize *= 2) {
          const copyEnd = Math.min(copySize * 2, rowWidth);
          pixels.copyWithin(rowStart + copySize, rowStart, rowStart + copyEnd - copySize);
        }
      }
    }
  } else {
    // 🚀 ULTRA-FAST FULL SCREEN CLEAR using copyWithin doubling strategy
    
    // Fast path for transparent black (common case)
    if (c[0] === 0 && c[1] === 0 && c[2] === 0 && c[3] === 0) {
      pixels.fill(0);
      return;
    }
    
    // Set the first pixel with the target color
    pixels[0] = c[0];     // r
    pixels[1] = c[1];     // g  
    pixels[2] = c[2];     // b
    pixels[3] = c[3];     // alpha
    
    // Use copyWithin to double the filled area repeatedly
    // This is much faster than for loops: O(log n) vs O(n) copies
    for (let copySize = 4; copySize < pixels.length; copySize *= 2) {
      const copyEnd = Math.min(copySize * 2, pixels.length);
      pixels.copyWithin(copySize, 0, copyEnd - copySize);
    }
  }
}

/**
 * Plot a single pixel using (x, y) or {x, y} if only x is given.
 * (1) {x, y}
 * (2) (x, y)
 */
// Where a pixel is a region in which we draw from the upper left corner. (2D)
function plot(x, y) {
  x = floor(x);
  y = floor(y);
  // Skip pixels that are offscreen and/or found in the `skips` list.
  if (x < 0 || x >= width || y < 0 || y >= height) return;

  // Check pixels in the active mask.
  if (activeMask) {
    // Don't apply pan translation to mask bounds when checking plot coordinates
    // The mask coordinates are already set relative to the current pan position
    if (
      y < activeMask.y ||
      y >= activeMask.y + activeMask.height ||
      x >= activeMask.x + activeMask.width ||
      x < activeMask.x
    )
      return;
  }

  for (const s of skips) if (x === s.x && y === s.y) return;
  // Plot our pixel.
  const i = (x + y * width) * 4;
  
  // Additional safety check for array bounds
  if (i < 0 || i >= pixels.length - 3) return;
  
  const alpha = c[3];

  // Erasing
  if (c[0] === -1 && c[1] === -1 && c[2] === -1) {
    erase(pixels, i, 1 - c[3] / 255);
  } else if (alpha === 255) {
    // No alpha blending, just copy.
    pixels.set(c, i);
  } else if (alpha !== 0) {
    blend(pixels, c, 0, i);
  }
}

// Adds a point to the skip list which ignores these points from being drawn
// in `plot`. Passing `null` will clear the skip list.
// TODO: Should the skip list clear automatically on every paint? 2022.02.03.01.16
// TODO: Leaving skip blank will default to a random skipping algorithm?
//       Writing a function will allow you to dynamically skip pixels.
function skip(...args) {
  if (args[0] === null) skips.length = 0;
  else
    args.forEach((p) => {
      skips.push({
        x: floor(p.x || p[0]) + panTranslation.x,
        y: floor(p.y || p[1]) + panTranslation.y,
      });
    });
}

// Plots a single pixel within the panned coordinate space.
// Basically a wrapper over plot, which should ultimately be renamed to set?
// Accepts x, y or {x, y}
function point(...args) {
  let x, y;

  if (args.length === 1) {
    if (args[0].length >= 2) {
      // Array
      x = args[0][0];
      y = args[0][1];
    } else {
      // Object
      x = args[0].x;
      y = args[0].y;
    }
  } else if (args.length >= 2) {
    // Multiple arguments
    x = args[0];
    y = args[1];
  } else {
    x = randInt(width);
    y = randInt(height);
  }

  // TODO: Add support for {x, y} single argument. 2022.02.02.20.39
  x += panTranslation.x;
  y += panTranslation.y;
  // TODO: Eventually add rotation and scale etc.

  plot(x, y);
  return [x, y];
}

// Run a callback function to shade, then plot an array of pixel coordinates.
// This runs within `pline` and `pixelPerfectPolyline`.
function shadePixels(points, shader, shaderArgs = []) {
  points.forEach((p) => {
    // TODO: - [] Send current pixel under p? This can be used for cool position
    //            reading color effects. 23.01.05.01.23

    // Clip
    if (p.x < 0) return;
    if (p.x >= width) return;
    if (p.y < 0) return;
    if (p.y >= height) return;

    shader.position?.(p, ...shaderArgs); // Compute position.

    // Clip again
    if (p.x < 0) return;
    if (p.x >= width) return;
    if (p.y < 0) return;
    if (p.y >= height) return;

    p.x = floor(p.x); // Floor position and check bounds.
    p.y = floor(p.y);

    const n = p.x + p.y * width;

    if (writeBuffer[n] !== 1) {
      writeBuffer[n] = 1; // Remember this point in the frame's writeBuffer.
      // 🪄 Pixel Shader
      const i = floor(p.x + p.y * width) * 4; // Get current pixel under p.
      const pixel = pixels.subarray(i, i + 4);
      // TODO: Put p.color into shaderArgs or some other automated thing?
      shader.color({ x: p.x, y: p.y }, pixel, c, p.color); // Modify color.
    }
  }); // Paint each filtered pixel.
}

function pan(x, y) {
  if (typeof x === "object") {
    x = x.x;
    y = x.y;
  }
  if (y === undefined) y = x;
  panTranslation.x += floor(x);
  panTranslation.y += floor(y);
}

function unpan() {
  panTranslation.x = 0;
  panTranslation.y = 0;
}

let savedPanTranslation;

// Save the local transform.
function savepan() {
  savedPanTranslation = { ...panTranslation };
}

// Restore it.
function loadpan() {
  if (savedPanTranslation) {
    panTranslation.x = savedPanTranslation.x;
    panTranslation.y = savedPanTranslation.y;
  }
}

// 😷 Mask off pixels.
function mask(box) {
  activeMask = box;
}

// 😄 Unmask pixels.
function unmask() {
  activeMask = null;
}

function copy(destX, destY, srcX, srcY, src, alpha = 1.0) {
  destX = floor(destX);
  destY = floor(destY);
  srcX = floor(srcX);
  srcY = floor(srcY);

  // Skip pixels that are offscreen or outside the src buffer.
  // Used in `paste`.
  if (
    destX < 0 ||
    destX >= width ||
    destY < 0 ||
    destY >= height ||
    srcX < 0 ||
    srcX >= src.width ||
    srcY < 0 ||
    srcY >= src.height
  ) {
    return;
  }

  const di = (destX + destY * width) * 4;
  const si = (srcX + srcY * src.width) * 4;

  blend(pixels, src.pixels, si, di, alpha);
}

/*
function copyRow(destX, destY, srcX, srcY, src) {
  destX = Math.round(destX);
  destY = Math.round(destY);
  srcX = Math.round(srcX);
  srcY = Math.round(srcY);

  const destIndex = (destX + destY * width) * 4;
  const srcIndex = (srcX + srcY * src.width) * 4;
  const rowLength = src.width * 4 - destX * 4;

  let srcStart = srcIndex;
  let srcEnd = srcIndex + src.width * 4;

  const sub = src.pixels.subarray(srcStart, srcEnd);

  pixels.set(sub, destIndex);
}
 */

// Resize a bitmap to a new with and height, returning a new
// bitmap, using nearest neighbor scaling.
// Bitmaps are {pixels: uint8array, width: int, height, int}
function resize(bitmap, width, height) {
  const ratioX = bitmap.width / width;
  const ratioY = bitmap.height / height;
  const pixels = new Uint8ClampedArray(width * height * 4);

  for (let y = 0; y < height; y += 1) {
    for (let x = 0; x < width; x += 1) {
      const index = (y * width + x) * 4;
      const srcX = floor(x * ratioX);
      const srcY = floor(y * ratioY);
      const srcIndex = (srcY * bitmap.width + srcX) * 4;

      pixels[index] = bitmap.pixels[srcIndex];
      pixels[index + 1] = bitmap.pixels[srcIndex + 1];
      pixels[index + 2] = bitmap.pixels[srcIndex + 2];
      pixels[index + 3] = bitmap.pixels[srcIndex + 3];
    }
  }
  return { pixels, width, height };
}

// Helper function to detect recording mode
function isInRecordingMode() {
  // Check multiple indicators that we're in recording mode
  return typeof globalThis !== 'undefined' && (
    globalThis.tapeRecording === true ||
    globalThis.recording === true ||
    (typeof process !== 'undefined' && process.argv && process.argv.includes('--recording'))
  );
}

// Fast checksum for pixel data (using sampling for performance)
function getPixelChecksum(pixels, width, height, x, y, w, h) {
  let checksum = 0;
  const sampleRate = Math.max(1, Math.floor(Math.sqrt(w * h) / 32)); // Sample ~32 points
  
  for (let sy = y; sy < y + h; sy += sampleRate) {
    for (let sx = x; sx < x + w; sx += sampleRate) {
      if (sx < width && sy < height) {
        const i = (sy * width + sx) * 4;
        checksum = (checksum * 31 + pixels[i] + pixels[i + 1] + pixels[i + 2] + pixels[i + 3]) >>> 0;
      }
    }
  }
  
  return checksum;
}

// Adjust the contrast of the pixel buffer
// level: 1.0 = no change, >1.0 = more contrast, <1.0 = less contrast
function contrast(level = 1.0) {
  // Early exit if no contrast change needed
  if (level === 1.0) return;
  
  // Determine the area to adjust (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Apply pan translation to mask bounds
    const maskX = activeMask.x + panTranslation.x;
    const maskY = activeMask.y + panTranslation.y;
    minX = Math.max(0, Math.floor(maskX));
    minY = Math.max(0, Math.floor(maskY));
    maxX = Math.min(width, Math.floor(maskX + activeMask.width));
    maxY = Math.min(height, Math.floor(maskY + activeMask.height));
  }

  // 🚀 OPTIMIZATION: Pre-calculate contrast lookup table for faster pixel processing
  const contrastLUT = new Uint8Array(256);
  for (let i = 0; i < 256; i++) {
    const normalized = i / 255.0;
    const adjusted = ((normalized - 0.5) * level + 0.5) * 255.0;
    contrastLUT[i] = Math.max(0, Math.min(255, Math.round(adjusted)));
  }

  // Apply contrast adjustment to each pixel using lookup table
  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const idx = (y * width + x) * 4;
      
      // Skip transparent pixels
      if (pixels[idx + 3] === 0) continue;
      
      // Use lookup table for fast contrast adjustment (RGB channels only)
      pixels[idx] = contrastLUT[pixels[idx]];     // R
      pixels[idx + 1] = contrastLUT[pixels[idx + 1]]; // G
      pixels[idx + 2] = contrastLUT[pixels[idx + 2]]; // B
      // Alpha channel stays unchanged
    }
  }
}

// Adjust brightness using lookup table optimization
// adjustment: -255 to +255 (0 = no change, positive = brighter, negative = darker)
function brightness(adjustment = 0) {
  // Early exit if no adjustment needed
  if (adjustment === 0) return;
  
  // Clamp adjustment to valid range
  adjustment = Math.max(-255, Math.min(255, adjustment));
  
  // Determine the area to adjust (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    const maskX = activeMask.x + panTranslation.x;
    const maskY = activeMask.y + panTranslation.y;
    minX = Math.max(0, Math.floor(maskX));
    minY = Math.max(0, Math.floor(maskY));
    maxX = Math.min(width, Math.floor(maskX + activeMask.width));
    maxY = Math.min(height, Math.floor(maskY + activeMask.height));
  }

  // 🚀 OPTIMIZATION: Pre-calculate brightness lookup table
  const brightnessLUT = new Uint8Array(256);
  for (let i = 0; i < 256; i++) {
    brightnessLUT[i] = Math.max(0, Math.min(255, i + adjustment));
  }

  // Apply brightness adjustment to each pixel using lookup table
  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const idx = (y * width + x) * 4;
      
      // Skip transparent pixels
      if (pixels[idx + 3] === 0) continue;
      
      // Use lookup table for fast brightness adjustment (RGB channels only)
      pixels[idx] = brightnessLUT[pixels[idx]];     // R
      pixels[idx + 1] = brightnessLUT[pixels[idx + 1]]; // G
      pixels[idx + 2] = brightnessLUT[pixels[idx + 2]]; // B
      // Alpha channel stays unchanged
    }
  }
}

// Copies pixels from a source buffer to the active buffer and returns
// the source buffer.
// TODO: Add dirty rectangle support here...
//       - What would the best parameter set be?
// `from` - can either be
// TODO: Replace with more generic algorithm?

// Notes:
// Scale can also be a transform object: { scale, angle }
// Blit only works with a scale of 1.
function paste(from, destX = 0, destY = 0, scale = 1, blit = false) {
  const pasteStart = performance.now(); // 🚨 PERFORMANCE TRACKING
  
  if (!from) {
    graphPerf.track('paste', 0);
    return;
  }

  // 🚨 ROBOT FIX: Ensure destination pixels buffer is available
  if (!pixels) {
    console.warn('🚨 paste: No destination pixels buffer available, skipping paste operation');
    graphPerf.track('paste', 0);
    return;
  }

  destX += panTranslation.x;
  destY += panTranslation.y;

  // 🚀 OPTIMIZATION: Skip paste operations for tiny scales
  if (typeof scale === "number" && Math.abs(scale) < 0.01) {
    graphPerf.track('paste', 0);
    return;
  }

  if (scale !== 1) {
    let angle = 0;
    let anchor;
    let width, height;

    if (typeof scale === "object") {
      angle = scale.angle;
      width = scale.width;
      height = scale.height;
      anchor = scale.anchor;
      // ^ Pull properties out of the scale object.
      scale = scale.scale; // And then redefine scale.
    }

    // Fast path for simple integer scaling (no rotation, no custom dimensions)
    if (
      !angle &&
      !width &&
      !height &&
      !anchor &&
      typeof scale === "number" &&
      scale > 0 &&
      scale === ~~scale &&
      scale <= 8
    ) {
      // Integer scale up to 8x for safety

      // Ultra-fast nearest-neighbor scaling using direct pixel manipulation
      const srcWidth = from.width;
      const srcHeight = from.height;
      const srcPixels = from.pixels;
      const scaleInt = ~~scale; // Convert to integer

      // Pre-calculate destination bounds
      const destWidth = srcWidth * scaleInt;
      const destHeight = srcHeight * scaleInt;

      // Boundary check
      if (
        destX >= 0 &&
        destY >= 0 &&
        destX + destWidth <= width &&
        destY + destHeight <= height
      ) {
        // Direct pixel buffer manipulation for maximum speed
        for (let srcY = 0; srcY < srcHeight; srcY += 1) {
          for (let srcX = 0; srcX < srcWidth; srcX += 1) {
            const srcIndex = (srcX + srcY * srcWidth) << 2; // Fast * 4

            if (srcIndex < srcPixels.length) {
              // Extract color data directly
              const r = srcPixels[srcIndex];
              const g = srcPixels[srcIndex + 1];
              const b = srcPixels[srcIndex + 2];
              const a = srcPixels[srcIndex + 3];

              // Skip transparent pixels for efficiency
              if (a > 0) {
                const baseDestX = destX + srcX * scaleInt;
                const baseDestY = destY + srcY * scaleInt;

                // Set color once and draw scaled block
                color(r, g, b, a);

                // Use box for efficiency when scale > 1
                if (scaleInt > 1) {
                  box(baseDestX, baseDestY, scaleInt, scaleInt, "fill");
                } else {
                  plot(baseDestX, baseDestY);
                }
              }
            }
          }
        }
        return; // Exit early for fast path
      }
    }

    // Fall back to general grid-based scaling for complex cases
    grid(
      {
        box: { x: destX, y: destY, w: from.width, h: from.height },
        transform: { scale, angle, width, height, anchor },
      },
      from,
    );

    return;
  }

  // Note: Angle does not work here yet... 23.04.29.23.38

  // TODO: See if from has a dirtyBox attribute.
  if (from.crop) {
    // A cropped copy - optimize with bulk pixel operations
    const cropW = from.crop.w;
    const cropH = from.crop.h;
    const fromPixels = from.painting?.pixels || from.pixels;

    // 🚀 OPTIMIZATION: Bulk pixel copying for cropped regions
    if (
      fromPixels &&
      destX >= 0 &&
      destY >= 0 &&
      destX + cropW <= width &&
      destY + cropH <= height
    ) {
      const fromWidth = from.painting?.width || from.width;
      
      for (let y = 0; y < cropH; y += 1) {
        const srcY = from.crop.y + y;
        const destRowY = destY + y;
        
        // Process entire row for better cache performance
        for (let x = 0; x < cropW; x += 1) {
          const srcX = from.crop.x + x;
          const srcIdx = (srcY * fromWidth + srcX) * 4;
          const destIdx = (destRowY * width + (destX + x)) * 4;
          
          // Skip if source index is out of bounds
          if (srcIdx + 3 < fromPixels.length) {
            const srcAlpha = fromPixels[srcIdx + 3];
            
            if (srcAlpha > 0) {
              if (srcAlpha === 255) {
                // Opaque source - direct copy
                pixels[destIdx] = fromPixels[srcIdx];
                pixels[destIdx + 1] = fromPixels[srcIdx + 1];
                pixels[destIdx + 2] = fromPixels[srcIdx + 2];
                pixels[destIdx + 3] = fromPixels[srcIdx + 3];
              } else {
                // Alpha blending
                const alpha = srcAlpha + 1;
                const invAlpha = 256 - alpha;
                pixels[destIdx] = (alpha * fromPixels[srcIdx] + invAlpha * pixels[destIdx]) >> 8;
                pixels[destIdx + 1] = (alpha * fromPixels[srcIdx + 1] + invAlpha * pixels[destIdx + 1]) >> 8;
                pixels[destIdx + 2] = (alpha * fromPixels[srcIdx + 2] + invAlpha * pixels[destIdx + 2]) >> 8;
                pixels[destIdx + 3] = Math.min(255, pixels[destIdx + 3] + srcAlpha);
              }
            }
          }
        }
      }
    } else {
      // Fall back to copy() function for edge cases
      for (let y = 0; y < cropH; y += 1) {
        for (let x = 0; x < cropW; x += 1) {
          copy(
            destX + x,
            destY + y,
            from.crop.x + x,
            from.crop.y + y,
            from.painting,
          );
        }
      }
    }
  } else {
    // Check to see if we can perform a full copy here,
    // with no alpha blending.
    if (blit) {
      pixels.set(from.pixels, 0);
    } else {
      // 🚀 MAJOR OPTIMIZATION: Bulk pixel copying for better performance
      const srcWidth = from.width;
      const srcHeight = from.height;
      const srcPixels = from.pixels;

      // Check if we can do ultra-fast bulk copying
      if (
        destX >= 0 &&
        destY >= 0 &&
        destX + srcWidth <= width &&
        destY + srcHeight <= height &&
        srcPixels
      ) {
        // Ultra-fast bulk pixel blending
        for (let y = 0; y < srcHeight; y += 1) {
          const srcRowStart = y * srcWidth * 4;
          const destRowStart = ((destY + y) * width + destX) * 4;
          
          // Process entire row at once for better cache performance
          for (let x = 0; x < srcWidth; x += 1) {
            const srcIdx = srcRowStart + (x * 4);
            const destIdx = destRowStart + (x * 4);
            
            // Skip transparent pixels
            if (srcPixels[srcIdx + 3] > 0) {
              // Fast alpha blending without function call overhead
              const srcAlpha = srcPixels[srcIdx + 3];
              
              if (srcAlpha === 255) {
                // Opaque source - direct copy (fastest path)
                pixels[destIdx] = srcPixels[srcIdx];
                pixels[destIdx + 1] = srcPixels[srcIdx + 1];
                pixels[destIdx + 2] = srcPixels[srcIdx + 2];
                pixels[destIdx + 3] = srcPixels[srcIdx + 3];
              } else {
                // Alpha blending (optimized inline version)
                const alpha = srcAlpha + 1;
                const invAlpha = 256 - alpha;
                pixels[destIdx] = (alpha * srcPixels[srcIdx] + invAlpha * pixels[destIdx]) >> 8;
                pixels[destIdx + 1] = (alpha * srcPixels[srcIdx + 1] + invAlpha * pixels[destIdx + 1]) >> 8;
                pixels[destIdx + 2] = (alpha * srcPixels[srcIdx + 2] + invAlpha * pixels[destIdx + 2]) >> 8;
                pixels[destIdx + 3] = Math.min(255, pixels[destIdx + 3] + srcAlpha);
              }
            }
          }
        }
      } else {
        // Fallback to copy() function for edge cases
        for (let y = 0; y < srcHeight; y += 1) {
          for (let x = 0; x < srcWidth; x += 1) {
            copy(destX + x, destY + y, x, y, from);
          }
        }
      }
    }
  }
  
  const pasteEnd = performance.now();
  graphPerf.track('paste', pasteEnd - pasteStart);
}

let stampSkipCounter = 0;

// Similar to `paste` but always centered.
// Fixed: Now properly supports scale and angle parameters while maintaining center positioning
function stamp(from, x, y, scale, angle) {
  const stampStart = performance.now(); // 🚨 PERFORMANCE TRACKING
  
  // � DEBUG: Log stamp calls
  // console.log(`🎯 STAMP called: from=${from?.constructor?.name || typeof from}, scale=${scale}, angle=${angle}`);
  
  // �🚀 OPTIMIZATION: Skip stamps that are too small to be visible
  if (scale !== undefined && Math.abs(scale) < 0.01) {
    graphPerf.track('stamp', 0);
    return;
  }

  // 🚀 PERFORMANCE OPTIMIZATION: Only skip frames when running very slowly
  if (graphPerf && graphPerf.lastFPS && graphPerf.lastFPS < 5) {
    stampSkipCounter++;
    if (stampSkipCounter % 3 !== 0) { // Skip 2 out of 3 frames when FPS < 5
      // console.log(`⏭️ STAMP skipped due to low FPS: ${graphPerf.lastFPS}`);
      graphPerf.track('stamp', 0);
      return;
    }
  } else {
    stampSkipCounter = 0; // Reset when performance is good
  }
  
  if (scale !== undefined || angle !== undefined) {
    // For stamp, we want to center at x,y coordinates
    // Account for scaling when calculating the center offset
    const effectiveScale = scale !== undefined ? scale : 1;
    const scaledWidth = from.width * effectiveScale;
    const scaledHeight = from.height * effectiveScale;
    
    // Adjust the position so that the scaled image is centered at our target x,y
    const adjustedX = x - scaledWidth / 2;
    const adjustedY = y - scaledHeight / 2;
    
    const transform = {};
    if (scale !== undefined) transform.scale = scale;
    if (angle !== undefined) transform.angle = angle;
    
    paste(from, adjustedX, adjustedY, transform);
  } else {
    // Fallback for simple case without scaling/rotation
    paste(from, x - from.width / 2, y - from.height / 2);
  }
  
  const stampEnd = performance.now();
  graphPerf.track('stamp', stampEnd - stampStart);
}

let blendingMode = "blend";
function blendMode(mode = "blend") {
  blendingMode = mode;
}

// let stipple = 0;
// A fast alpha blending function that looks into a pixel array.
// Transcribed from C++: https://stackoverflow.com/a/12016968
function blend(dst, src, si, di, alphaIn = 1) {
  //stipple += 1;
  //if (stipple < 4) { return; }
  //stipple = 0;

  if (blendingMode === "erase") {
    const normalAlpha = 1 - src[si + 3] / 255;
    dst[di + 3] *= normalAlpha;
    if (dst[di + 3] === 0) {
      // If the alpha is zero then wipe the data.
      dst[di] = 32;
      dst[di + 1] = 32;
      dst[di + 2] = 32;
    }
    return;
  }

  if (src[si + 3] === 0) return; // Return early if src is invalid.

  // Just do a straight up copy if we are in "blit" mode.
  if (blendingMode === "blit") {
    for (let i = 0; i < 4; i++) {
      if (i != 3) {
        dst[di + i] = src[si + i]; // For R, G, B channels
      } else {
        dst[di + i] = src[si + i] * alphaIn; // For the Alpha channel
      }
    }
    return;
  }

  // A. Blend over transparent pixels.
  if (dst[di + 3] < 255 && src[si + 3] > 0) {
    const epsilon = 1e-10; // Small number to prevent division by zero
    const alphaSrc = (src[si + 3] * alphaIn) / 255;
    const alphaDst = dst[di + 3] / 255;
    const combinedAlpha = alphaSrc + (1.0 - alphaSrc) * alphaDst;
    if (combinedAlpha > epsilon) {
      // Check against a very small number instead of 0
      for (let offset = 0; offset < 3; offset++) {
        // Iterate over R, G, B channels
        dst[di + offset] =
          (src[si + offset] * alphaSrc +
            dst[di + offset] * (1.0 - alphaSrc) * alphaDst) /
          (combinedAlpha + epsilon); // Add epsilon to prevent division instability
      }
      dst[di + 3] = combinedAlpha * 255;
    }
  } else {
    // B. Blend over opaque pixels.
    const alpha = src[si + 3] * alphaIn + 1;
    const invAlpha = 256 - alpha;
    dst[di] = (alpha * src[si + 0] + invAlpha * dst[di + 0]) >> 8;
    dst[di + 1] = (alpha * src[si + 1] + invAlpha * dst[di + 1]) >> 8;
    dst[di + 2] = (alpha * src[si + 2] + invAlpha * dst[di + 2]) >> 8;
    // Keep destination alpha opaque when blending over opaque background
    dst[di + 3] = 255;
  }
}

// Blends the alpha channel only / erases pixels.
function erase(pixels, i, normalizedAlpha) {
  pixels[i + 3] *= normalizedAlpha;
}

// Draws a horizontal line. (Should be very fast...)
function lineh(x0, x1, y) {
  x0 = floor(x0);
  x1 = floor(x1);
  y = floor(y);
  if (y < 0 || y >= height || x0 >= width || x1 < 0) return; // Check if the entire line is outside the mask
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    if (
      y < activeMask.y ||
      y >= activeMask.y + activeMask.height ||
      x1 < activeMask.x ||
      x0 >= activeMask.x + activeMask.width
    )
      return;
  }

  // Clamp to screen bounds first
  x0 = clamp(x0, 0, width - 1);
  x1 = clamp(x1, 0, width - 1); // Then clamp to mask bounds if mask is active
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    x0 = clamp(x0, activeMask.x, activeMask.x + activeMask.width - 1);
    x1 = clamp(x1, activeMask.x, activeMask.x + activeMask.width - 1);
  }

  const firstIndex = (x0 + y * width) * 4;
  const secondIndex = (x1 + y * width) * 4;

  let startIndex, endIndex;

  // Sort indices so we can always draw from left to right.
  if (firstIndex > secondIndex) {
    startIndex = secondIndex;
    endIndex = firstIndex;
  } else {
    startIndex = firstIndex;
    endIndex = secondIndex;
  }

  // Erasing.
  if (blendMode !== "erase" && c[0] === -1 && c[1] === -1 && c[2] === -1) {
    const normalAlpha = 1 - c[3] / 255;
    for (let i = startIndex; i <= endIndex; i += 4) {
      erase(pixels, i, normalAlpha);
    }
    // No alpha.
  } else if (c[3] === 255) {
    for (let i = startIndex; i <= endIndex; i += 4) {
      pixels[i] = c[0];
      pixels[i + 1] = c[1];
      pixels[i + 2] = c[2];
      pixels[i + 3] = 255;
    }
    // Alpha blending.
  } else if (c[3] !== 0) {
    for (let i = startIndex; i <= endIndex; i += 4) {
      blend(pixels, c, 0, i);
    }
  }
}

// Draws a line
// (2) p1, p2: pairs of {x, y} or [x, y]
// (4) x0, y0, x1, y1
// TODO: Automatically use lineh if possible. 22.10.05.18.27
function line() {
  let x0, y0, x1, y1;
  if (arguments.length === 1) {
    // Safely access properties on the first argument
    const arg0 = arguments[0];
    if (arg0 && typeof arg0 === "object") {
      x0 = arg0.x0; // Assume an object { x0, y0, x1, y1 }
      y0 = arg0.y0;
      x1 = arg0.x1;
      y1 = arg0.y1;
    }
  } else if (arguments.length === 4) {
    x0 = arguments[0]; // Set all `undefined` or `null` values to 0.
    y0 = arguments[1];
    x1 = arguments[2];
    y1 = arguments[3];
  } else if (arguments.length === 2) {
    const arg0 = arguments[0];
    const arg1 = arguments[1];
    if (Array.isArray(arg0) && Array.isArray(arg1)) {
      // assume [x, y], [x, y]
      x0 = arg0[0];
      y0 = arg0[1];
      x1 = arg1[0];
      y1 = arg1[1];
    } else if (
      arg0 &&
      typeof arg0 === "object" &&
      arg1 &&
      typeof arg1 === "object"
    ) {
      // assume {x, y}, {x, y}
      x0 = arg0.x;
      y0 = arg0.y;
      x1 = arg1.x;
      y1 = arg1.y;
    }
  } else {
    // if (debug) {
    //   console.warn(
    //     "Line did not use the correct number of arguments:",
    //     arguments
    //   );
    // }
  }

  // Set all untruthy values like null, or undefined to a random value.
  if (x0 == null) x0 = randIntRange(0, width);
  if (y0 == null) y0 = randIntRange(0, height);
  if (x1 == null) x1 = randIntRange(0, width);
  if (y1 == null) y1 = randIntRange(0, height);

  // Handle NaN values the same way as null - use random values
  if (isNaN(x0)) x0 = randIntRange(0, width);
  if (isNaN(y0)) y0 = randIntRange(0, height);
  if (isNaN(x1)) x1 = randIntRange(0, width);
  if (isNaN(y1)) y1 = randIntRange(0, height);

  // console.log("Line in:", x0, y0, x1, y1);

  // Add any panTranslations.
  x0 += panTranslation.x;
  y0 += panTranslation.y;
  x1 += panTranslation.x;
  y1 += panTranslation.y;

  // Lerp from primary to secondary color as needed.
  const cachedInk = c.slice(0);

  // Check if line is perfectly horizontal and no gradient/fade is present, otherwise run bresenham.
  if (y0 === y1 && !c2 && !fadeMode) {
    lineh(x0, x1, y0);
  } else {
    const lineLength = sqrt((x1 - x0) ** 2 + (y1 - y0) ** 2);
    
    bresenham(x0, y0, x1, y1).forEach((p, index, points) => {
      if (fadeMode) {
        // Calculate fade position based on line progress
        const t = lineLength > 0 ? sqrt((p.x - x0) ** 2 + (p.y - y0) ** 2) / lineLength : 0;
        const fadeColor = getFadeColor(t, p.x, p.y);
        setColor(...fadeColor);
        plot(p.x, p.y);
      } else if (c2) {
        const step = sqrt(p.x * p.x + p.y * p.y) / 255; // Gradient step.
        color(...shiftRGB(c, c2, step));
        plot(p.x, p.y);
      } else {
        plot(p.x, p.y);
      }
    });
    if (c2 || fadeMode) setColor(...cachedInk);
  }

  const out = [x0, y0, x1, y1];
  // console.log("Line out:", out);
  twoDCommands?.push(["line", ...out]); // Forward this call to the GPU.
  return out;
}

// Takes an array of pixel coords `{x, y}` and filters out L shapes.
// Note: It checks the previous, current, and next pixel and requires a minimum
//        set of 3 before it removes anything.
// Draws a regular `line` if only two pixels are provided.
// Transcribed from: https://rickyhan.com/jekyll/update/2018/11/22/pixel-art-algorithm-pixel-perfect.html
function pixelPerfectPolyline(points, shader) {
  if (points.length < 2) return; // Require 2 or more points.
  const pixels = [];
  //points.reverse(); // Draw from front to back.

  let last = points[0];

  // TODO: Draw everything front to back similar to pline? 23.02.01.13.12
  //for (let i = coords.length - 2; i >= 0; i -= 1) {

  points.forEach((cur) => {
    // Clip offscreen segments.
    const xMin = min(last.x, cur.x);
    const xMax = max(last.x, cur.x);
    const yMin = min(last.y, cur.y);
    const yMax = max(last.y, cur.y);
    if (xMin >= width || xMax < 0 || yMin >= height || yMax < 0) {
      last = cur;
      return;
    }

    //if (cur.color === "rainbow") color(rainbow());

    const rb = last.color === "rainbow";

    // Compute bresen pixels, filtering out duplicates.
    bresenham(last.x, last.y, cur.x, cur.y).forEach((p, i) => {
      if (i > 0 || pixels.length < 2) {
        pixels.push({ ...p, color: rb ? rainbow() : last.color }); // Add color for each pixel.
      }
    });
    last = cur;
  });

  const filtered = [];
  let c = 0;

  while (c < pixels.length) {
    if (
      c > 0 &&
      c + 1 < pixels.length &&
      (pixels[c - 1].x === pixels[c].x || pixels[c - 1].y === pixels[c].y) && // check left and up
      (pixels[c + 1].x === pixels[c].x || pixels[c + 1].y === pixels[c].y) && // check right and down
      pixels[c - 1].x !== pixels[c + 1].x && // check left and right of prev and next
      pixels[c - 1].y !== pixels[c + 1].y
    ) {
      // check top and bottom of prev and next
      c += 1;
    }
    filtered.push(pixels[c]);
    c += 1;
  }

  if (shader) {
    shadePixels(filtered, shader);
  } else {
    filtered.forEach((p) => point(p));
  }
}

// Draws a line from a point at a distance... with an angle in degrees.
function lineAngle(x1, y1, dist, degrees) {
  const x2 = x1 + dist * cos(radians(degrees));
  const y2 = y1 + dist * sin(radians(degrees));
  return line(x1, y1, x2, y2);
}

// Take two vertices and plot a 3d line with depth information.
function line3d(a, b, lineColor, gradients) {
  const aColor = a.color;
  const bColor = b.color;

  a = a.transform(screenMatrix);
  b = b.transform(screenMatrix);

  const aZ = a.pos[Z];
  const bZ = a.pos[Z];

  a = a.perspectiveDivide();
  b = b.perspectiveDivide();

  const [x0, y0, z0] = a.pos;
  const [x1, y1, z1] = b.pos;
  const points = bresenham(x0, y0, x1, y1);

  const saveColor = c.slice();

  color(...lineColor); // Set color from lineColor or default to global color.
  // color(randInt(255), randInt(255), randInt(255)); // Random colors.

  points.forEach((p, i) => {
    const progress = i / points.length;
    //const z = lerp(z0, z1, progress);

    // Lerp from a.color to b.color if gradients are enabled.
    if (gradients && aColor && bColor) {
      const R = lerp(aColor[0], bColor[0], progress);
      const G = lerp(aColor[1], bColor[1], progress);
      const B = lerp(aColor[2], bColor[2], progress);
      color(R, G, B);
    }

    const newZ = lerp(aZ, bZ, progress);
    const stretchedDepth = 1 - min(1, abs(newZ) / 2.5);

    /*
    color(
      c[0] * stretchedDepth,
      c[1] * stretchedDepth,
      c[2] * stretchedDepth,
    ); // Set color from lineColor or default to global color.
    */

    const depth = newZ * -1;
    if (depthBuffer) {
      const index = p.x + p.y * width;
      if (depth > depthBuffer[index]) {
      } else {
        depthBuffer[index] = depth;
        plot(p.x, p.y);
      }
    } else {
      plot(p.x, p.y);
    }
  });

  color(...saveColor); // Restore color.
}

// TODO: Implement a nice filled option here...
//       Something pixel-perfect with the outline... like a flood?
function circle(x0, y0, radius, filled = false, thickness, precision) {
  if (filled || thickness > 1) {
    oval(x0, y0, radius, radius, filled, thickness, precision);
    return;
  }

  // Circle
  x0 = floor(x0);
  y0 = floor(y0);
  radius = floor(radius);

  let f = 1 - radius,
    ddF_x = 0,
    ddF_y = -2 * radius,
    x = 0,
    y = radius;

  point(x0, y0 + radius);
  point(x0, y0 - radius);
  point(x0 + radius, y0);
  point(x0 - radius, y0);

  while (x < y) {
    if (f >= 0) {
      y -= 1;
      ddF_y += 2;
      f += ddF_y;
    }
    x += 1;
    ddF_x += 2;
    f += ddF_x + 1;
    point(x0 + x, y0 + y);
    point(x0 - x, y0 + y);
    point(x0 + x, y0 - y);
    point(x0 - x, y0 - y);
    point(x0 + y, y0 + x);
    point(x0 - y, y0 + x);
    point(x0 + y, y0 - x);
    point(x0 - y, y0 - x);
  }
}

// TODO: Generate sampled points around a circle then use
function oval(
  x0,
  y0,
  radiusX,
  radiusY,
  filled = false,
  thickness = 1,
  precision,
) {
  const points = generateEllipsePoints(x0, y0, radiusX, radiusY, precision);
  shape({ points, filled, thickness });
}

// TODO: How can I relate precision to the circumference and avoid this little
//       sharp point on the top?
// TODO: This function can have radians etc. / be more terse. 23.02.13.19.42
function generateEllipsePoints(x0, y0, radiusX, radiusY, precision = 20) {
  const points = [];
  for (let i = 0; i < 360; i += precision) {
    const angle = radians(i);
    const x = x0 + radiusX * cos(angle);
    const y = y0 + radiusY * sin(angle);
    points.push([x, y]);
  }
  return points;
}

// Draws a series of 1px lines without overlapping / overdrawing points.
// TODO: Add closed mode? Example: ink(handPalette.w).poly([...w, w[0]]);
function poly(coords) {
  let last = coords[0];
  for (let i = 1; i < coords.length; i++) {
    const cur = coords[i];
    line(last, cur);
    last = cur;
  }
}

// Rasterize an Npx thick poly line with rounded end-caps.
/* TODO
 - [😇] Clip coords to inside of the screen.
 + Later
 - [] Perhaps if thickness === 1 then this can be combined with
     `pixelPerfectPolyline` ?
 - [] Render a third triangle from mid point to last point to next quad
      point?
 - [] Rounded half-circle endcaps.
 - [] Filled circle if coords.length === 1.
 - [] Texture / special FX.
 + Done
 - [x] Triangle rasterization of segment.
 - [x] Optimize performance.
   - [x] Run the profiler.
*/
function pline(coords, thickness, shader) {
  // 1️⃣ Generate geometry.
  if (coords.length < 2) return; // Require at least two coordinates.

  let points = [], // Raster grids.
    lines = [],
    tris = [];

  // 🎴 Draw everything from front to back!

  let last = coords[coords.length - 1]; // Keep the last drawn element.

  let lpar, ldir; // Store last parallel points / prepopulate if supplied.

  for (let i = coords.length - 2; i >= 0; i -= 1) {
    const cur = coords[i];

    // 1. Two points on both sides of last and cur via line dir.
    const lp = [last.x || last[0], last.y || last[1]],
      cp = [cur.x || cur[0], cur.y || cur[1]]; // Convert last and cur to vec2.

    const dir = vec2.normalize([], vec2.subtract([], cp, lp)); // Line direction.
    if (!ldir) ldir = dir;

    const rot = vec2.rotate([], dir, [0, 0], PI / 2); // Rotated by 90d

    const offset1 = vec2.scale([], rot, thickness / 2); // Parallel offsets.
    const offset2 = vec2.scale([], rot, -thickness / 2);

    let c1, c2;
    if (!lpar) {
      c1 = vec2.add([], lp, offset1); // Compute both sets of points.
      c2 = vec2.add([], lp, offset2);
      lpar = [c1, c2];
    } else {
      [c1, c2] = lpar;
    }

    [c1, c2, lp, cp].forEach((v) => vec2.floor(v, v)); // Floor everything.

    // 2. Plotting

    const dot = vec2.dot(dir, ldir); // Get the dot product of cur and last dir.

    let trig; // Triangle geometry.

    if (dot > 0) {
      // Vertex order for forward direction.
      trig = [
        [c1, c2, lp],
        [c2, lp, cp],
      ];
      lines.push(...bresenham(...c1, ...lp)); // Par line 1
      lines.push(...bresenham(...c2, ...cp)); // Par line 2
    } else {
      // Vertex order for backward direction.
      trig = [
        [c2, lp, c1],
        [c1, cp, lp],
      ];
      lines.push(...bresenham(...c2, ...lp)); // Par line 1
      lines.push(...bresenham(...c1, ...cp)); // Par line 2
    }

    // Partial outside clipping.
    // const clippedTris = trig.filter((triangle) =>
    //   triangle.every(
    //     (v) => v[0] >= 0 && v[0] < width && v[1] >= 0 && v[1] < height
    //   )
    // );

    // Full outside clipping.
    // Clip triangles that are *fully* offscreen.
    // Take into account panning here...
    const clippedTris = trig.filter((triangle) =>
      triangle.some((v) => {
        const tv = v.slice();
        tv[0] += panTranslation.x;
        tv[1] += panTranslation.y;
        return tv[0] >= 0 && tv[0] < width && tv[1] >= 0 && tv[1] < height;
      }),
    );

    clippedTris.forEach((tri) => fillTri(tri, tris)); // Fill quad.
    //trig.forEach((t) => fillTri(t, tris)); // Fill quad.

    ldir = dir;
    lines.push(...bresenham(...lp, ...cp));

    if (i === coords.length - 2)
      points.push({ x: c1[0], y: c1[1] }, { x: c2[0], y: c2[1] });

    points.push({ x: lp[0], y: lp[1] }, { x: cp[0], y: cp[1] }); // Add points.

    // Paint each triangle.
    if (cur.color === "rainbow") color(...rainbow());
    else if (cur.color) color(...cur.color);

    if (shader) {
      const progress = 1 - i / (coords.length - 2);
      shadePixels(tris, shader, [progress]);
    } else {
      tris.forEach((p) => point(p));
    }

    tris.length = 0;

    last = cur; // Update the last point.
    lpar = [c1, c2]; // ... and last parallel points.
  }

  // 3️⃣ Painting

  // color(0, 255, 0); // Paint vertex points.
  // points.forEach((p) => point(p));

  // color(0, 0, 255); // Paint wireframe lines.
  // lines.forEach((p) => point(p));

  return lpar;
}

// 🔺 Rasterizes a tri. See also: https://www.youtube.com/watch?v=SbB5taqJsS4
function fillTri(v3, pix) {
  const scan = []; // Scan buffer of left->right down Y axis.
  const [min, mid, max] = v3.sort((va, vb) => va[1] - vb[1]); // Sort Y.

  const v1 = [max[0] - min[0], max[1] - min[1]], // Area / cross-product.
    v2 = [mid[0] - min[0], mid[1] - min[1]],
    cp = v1[0] * v2[1] - v2[0] * v1[1];

  const handedness = cp > 0 ? 1 : cp < 0 ? 0 : -1; // O->L, 1->R, -1->️🚫

  // Scan across each edge.
  [
    [min, max, 0 + handedness], // Min and Max for each edge with handedness.
    [min, mid, 1 - handedness],
    [mid, max, 1 - handedness],
  ].forEach(function scanEdge(v) {
    const yStart = v[0][Y],
      yEnd = v[1][Y],
      xStart = v[0][X],
      xEnd = v[1][X];

    const yDist = yEnd - yStart;
    const xDist = xEnd - xStart;
    if (yDist <= 0) return; // Don't convert if we have no columns.
    const xStep = xDist / yDist;

    let x = xStart;
    for (let y = yStart; y < yEnd; y += 1) {
      scan[y * 2 + v[2]] = floor(x);
      x += xStep;
    }
  });

  // Rasterize all by scanning horizontally.
  for (let y = min[Y]; y < max[Y]; y += 1) {
    for (let x = scan[y * 2]; x < scan[y * 2 + 1]; x += 1) {
      pix.push({ x, y });
    }
  }
}

/**
 * Bresenham's Line Algorithm
 * @description - Returns an array of integer points that make up an aliased line from {x0, y0} to {x1, y1}.
 * - This function is "abstract" and does not render anything... but outputs points.
 * @param x0
 * @param y0
 * @param x1
 * @param y1
 * @returns {*[]}
 */
function bresenham(x0, y0, x1, y1) {
  const points = [];

  // Make sure everything is floor'd.
  x0 = floor(x0) || 0;
  y0 = floor(y0) || 0;
  x1 = floor(x1) || 0;
  y1 = floor(y1) || 0;
  // Bresenham's Algorithm
  const dx = abs(x1 - x0);
  const dy = -abs(y1 - y0);
  const sx = x0 < x1 ? 1 : -1;
  const sy = y0 < y1 ? 1 : -1;
  let err = dx + dy;

  while (true) {
    points.push({ x: x0, y: y0 });

    if (x0 === x1 && y0 === y1) break;
    const e2 = 2 * err;
    if (e2 >= dy) {
      err += dy;
      x0 += sx;
    }
    if (e2 <= dx) {
      err += dx;
      y0 += sy;
    }
  }
  return points;
}

// Takes in x, y, width and height and draws an
// outline, inline (1px) or filled rectangle, optionally
// from the center by inputting eg: "inline*center" in mode.
const BOX_CENTER = "center";
// Parameters
// (1) box (any object with {x, y, w, h} properties) (1)
// (2) box, mode (2)
// (3) x, y, size (3)
// (4) x, y, w, h (4)
// (4) x, y, size, mode:string (4)
// (5) x, y, w, h, mode (5)
// TODO: ♾️ If width or height (size) is infinity then make sure the box covers
//          the entire pixel buffer width. 25.05.11.16.30
function box() {
  const boxStart = performance.now(); // 🚨 PERFORMANCE TRACKING
  
  let x,
    y,
    w,
    h,
    mode = "fill";

  // Apply the TODO: If any argument is NaN then just make it 'undefined'
  for (let i = 0; i < arguments.length; i++) {
    if (Number.isNaN(arguments[i])) {
      arguments[i] = undefined;
    }
  }

  if (arguments.length === 1 || arguments.length === 2) {
    // Array(4)
    if (Array.isArray(arguments[0])) {
      x = arguments[0][0];
      y = arguments[0][1];
      w = arguments[0][2];
      h = arguments[0][3];
    } else if (arguments[0]) {
      // Object {x, y, w, h}
      // Note: Also works with anything that has width and height properties.

      // Allow long names.
      if (!isNaN(arguments[0].width)) arguments[0].w = arguments[0].width;
      if (!isNaN(arguments[0].height)) arguments[0].h = arguments[0].height;

      x = arguments[0].x || 0;
      y = arguments[0].y || 0;
      w = arguments[0].w || 0;

      if (isNaN(arguments[0].h)) {
        h = w;
      } else {
        h = arguments[0].h;
      }

      if (x === undefined || y === undefined || w === undefined) {
        return console.error(
          "Could not make a box {x,y,w,h} from:",
          arguments[0],
        );
      }
    }
    if (arguments[1]) mode = arguments[1];
  } else if (arguments.length === 3) {
    // x, y, size
    x = arguments[0];
    y = arguments[1];
    w = arguments[2];
    h = arguments[2];
  } else if (arguments.length === 4) {
    if (typeof arguments[3] === "number") {
      // x, y, w, h
      x = arguments[0];
      y = arguments[1];
      w = arguments[2];
      h = arguments[3];
    } else {
      // x, y, size, mode
      x = arguments[0];
      y = arguments[1];
      w = arguments[2];
      h = arguments[2];
      mode = arguments[3];
    }
  } else if (arguments.length === 5) {
    // x, y, w, h, mode
    x = arguments[0];
    y = arguments[1];
    w = arguments[2];
    h = arguments[3];
    mode = arguments[4];
  } else {
    //return console.error("Invalid box call.");
  }

  if (w === 0 || h === 0 || isNaN(w) || isNaN(h)) return; // Silently quit if the box has no volume.
  if (w === Infinity) w = width;
  if (h === Infinity) h = height;

  // Random parameters if undefined, null, or NaN.
  if (nonvalue(x)) x = randInt(width);
  if (nonvalue(y)) y = randInt(height);
  if (nonvalue(w)) w = randInt(width);
  if (nonvalue(h)) h = randInt(height);

  // Abs / normalize the parameters.
  if (mode === undefined || mode === "") mode = "fill";

  // Check for "Center" at the end of mode.
  if (mode.endsWith(BOX_CENTER)) {
    x -= w / 2;
    y -= h / 2;
    mode = mode.slice(0, -BOX_CENTER.length - 1); // Clear through separator: *
  }

  x = floor(x);
  y = floor(y);
  w = signedCeil(w);
  h = signedCeil(h);

  ({ x, y, w, h } = Box.from([x, y, w, h]).abs);

  // Apply any global pan translations.
  // x += panTranslation.x; // Note: Already processed in `line`.
  // y += panTranslation.y;

  const thickness = parseInt(mode.split(":")[1]) || 1;
  mode = mode.split(":")[0];

  if (mode === "outline" || mode === "out") {
    if (thickness === 1) {
      line(x - 1, y - 1, x + w, y - 1); // Top
      line(x - 1, y + h, x + w, y + h); // Bottom
      line(x - 1, y, x - 1, y + h - 1); // Left
      line(x + w, y, x + w, y + h - 1); // Right
    } else {
      const leftX = x - thickness;
      const topY = y - thickness;
      const rightX = x + w + thickness;
      const bottomY = y + h + thickness;
      const boxHeight = h + thickness * 2;
      const boxWidth = w + thickness * 2;
      box(leftX, topY, boxWidth, thickness); // Top box
      box(leftX, bottomY - thickness, boxWidth, thickness); // Bottom box
      box(leftX, topY + thickness, thickness, boxHeight - thickness * 2); // Left box
      box(
        rightX - thickness,
        topY + thickness,
        thickness,
        boxHeight - thickness * 2,
      ); // Right box
    }
  } else if (mode === "inline" || mode === "in") {
    if (thickness === 1) {
      line(x, y, x + w - 1, y); // Top
      line(x, y + h - 1, x + w - 1, y + h - 1); // Bottom
      line(x, y + 1, x, y + h - 2); // Left
      line(x + w - 1, y + 1, x + w - 1, y + h - 2); // Right
    } else {
      if (thickness * 2 <= w && thickness * 2 <= h) {
        box(x, y, w, thickness); // Top
        box(x, y + h - thickness, w, thickness); // Bottom
        box(x, y + thickness, thickness, h - thickness * 2); // Left
        box(x + w - thickness, y + thickness, thickness, h - thickness * 2); // Right
      } else {
        box(x, y, w, h); // Just fill the box if the inline is too big.
      }
    }
  } else if (mode === "fill" || mode === "") {
    // TODO: The boxes could be cropped to always fit inside the screen here.
    w -= 1;
    const cachedInk = c.slice(0);
    
    // 🎨 NEW LOCAL FADE HANDLING: Check if current color is a fade
    const fadeInfo = parseFadeColor(c);
    const isLocalFade = fadeInfo !== null;
    
    // console.log("📦 BOX DEBUG: Local fade detection", {
    //   "current color c": c,
    //   "c[0]": c[0],
    //   "typeof c[0]": typeof c[0],
    //   "c[0] startsWith fade": typeof c[0] === 'string' ? c[0].startsWith('fade:') : 'not string',
    //   "fadeInfo": fadeInfo,
    //   "isLocalFade": isLocalFade,
    //   "global fadeMode": fadeMode,
    //   "global fadeColors": fadeColors.length > 0 ? fadeColors : "empty"
    // });
    
    if (sign(height) === 1) {
      for (let row = 0; row < h; row += 1) {
        if (isLocalFade) {
          // Use local fade info instead of global fade state
          const numericAngle = parseFloat(fadeInfo.direction);
          
          if (!isNaN(numericAngle)) {
            // For numeric angles, we need to calculate fade per pixel
            // Draw line pixel by pixel with angle-based fade
            for (let col = 0; col <= w; col++) {
              const pixelX = x + col;
              const pixelY = y + row;
              
              // Use the same calculateAngleFadePosition function as clear()
              const t = calculateAngleFadePosition(pixelX, pixelY, x, y, x + w, y + h - 1, numericAngle);
              
              // Calculate fade color locally without setting global state
              const fadeColor = getLocalFadeColor(t, pixelX, pixelY, fadeInfo);
              setColor(...fadeColor);
              plot(pixelX, pixelY);
            }
          } else {
            // Use named direction
            let t;
            if (fadeInfo.direction === "vertical") {
              t = h <= 1 ? 0 : row / (h - 1);
            } else if (fadeInfo.direction === "vertical-reverse") {
              t = h <= 1 ? 0 : 1 - (row / (h - 1)); // Reverse the direction
            } else if (fadeInfo.direction === "horizontal") {
              t = 0; // Will be calculated per pixel in line drawing
            } else if (fadeInfo.direction === "horizontal-reverse") {
              t = 0; // Will be calculated per pixel in line drawing
            } else { // diagonal, etc.
              t = h <= 1 ? 0 : row / (h - 1);
            }
            
            if (fadeInfo.direction.startsWith("horizontal")) {
              // For horizontal fades, draw pixels individually with local fade colors
              const lineY = y + row;
              for (let px = x; px < x + w; px++) {
                // Prevent division by zero for single-pixel width
                const horizontalT = w <= 1 ? 0 : (
                  fadeInfo.direction === "horizontal-reverse" 
                    ? 1 - ((px - x) / (w - 1))
                    : (px - x) / (w - 1)
                );
                const fadeColor = getLocalFadeColor(horizontalT, px, lineY, fadeInfo);
                setColor(...fadeColor);
                plot(px, lineY);
              }
            } else {
              // For vertical fades, set color once per row
              const fadeColor = getLocalFadeColor(t, x, y + row, fadeInfo);
              setColor(...fadeColor);
              line(x, y + row, x + w, y + row);
            }
          }
        } else {
          line(x, y + row, x + w, y + row);
        }
      }
    } else {
      for (let row = 0; row > h; row -= 1) {
        if (isLocalFade) {
          // Use local fade info instead of global fade state
          const numericAngle = parseFloat(fadeInfo.direction);
          
          if (!isNaN(numericAngle)) {
            // For numeric angles, we need to calculate fade per pixel
            // Draw line pixel by pixel with angle-based fade
            for (let col = 0; col <= w; col++) {
              const pixelX = x + col;
              const pixelY = y + row;
              
              // Use the same calculateAngleFadePosition function as clear()
              const t = calculateAngleFadePosition(pixelX, pixelY, x, y, x + w, y + h - 1, numericAngle);
              
              const fadeColor = getLocalFadeColor(t, pixelX, pixelY, fadeInfo);
              setColor(...fadeColor);
              plot(pixelX, pixelY);
            }
          } else {
            // Use named direction
            let t;
            if (fadeInfo.direction === "vertical") {
              t = Math.abs(row) / Math.abs(h - 1);
            } else if (fadeInfo.direction === "vertical-reverse") {
              t = 1 - (Math.abs(row) / Math.abs(h - 1));
            } else if (fadeInfo.direction === "horizontal") {
              t = 0; // Will be calculated per pixel in line drawing
            } else if (fadeInfo.direction === "horizontal-reverse") {
              t = 0; // Will be calculated per pixel in line drawing
            } else { // diagonal, etc.
              t = Math.abs(row) / Math.abs(h - 1);
            }
            
            if (fadeInfo.direction.startsWith("horizontal")) {
              // For horizontal fades, we need to temporarily set up fade state for line()
              // TODO: Update line() function to also use local fade handling
              const savedFadeMode = fadeMode;
              const savedFadeColors = fadeColors.slice();
              const savedFadeDirection = fadeDirection;
              const savedFadeNeat = fadeNeat;
              
              fadeMode = true;
              fadeColors = fadeInfo.colors;
              fadeDirection = fadeInfo.direction;
              fadeNeat = fadeInfo.neat;
              
              line(x, y + row, x + w, y + row);
              
              // Restore previous fade state
              fadeMode = savedFadeMode;
              fadeColors = savedFadeColors;
              fadeDirection = savedFadeDirection;
              fadeNeat = savedFadeNeat;
            } else {
              // For vertical fades, set color once per row
              const fadeColor = getLocalFadeColor(t, x, y + row, fadeInfo);
              setColor(...fadeColor);
              line(x, y + row, x + w, y + row);
            }
          }
        } else {
          line(x, y + row, x + w, y + row);
        }
      }
    }
  }
  
  graphPerf.track('box', performance.now() - boxStart); // 🚨 PERFORMANCE TRACKING
}

// Rasterizes an outlined or filled shape from pairs of points.
// Accepts: (x, y, x, y, x, y, ...)
//      Or: ([[x, y], [x, y], ...])
//      Or: { points: ", filled: false }
function shape() {
  const shapeStart = performance.now(); // 🚨 PERFORMANCE TRACKING
  
  let argPoints;
  let points;
  let filled = true;
  let thickness = 1; // Used if unfilled.

  if (arguments.length === 1 && !Array.isArray(arguments[0])) {
    // Assume an object {points, filled}
    argPoints = arguments[0].points;
    filled = arguments[0].filled;
    thickness = arguments[0].thickness || thickness;
  } else {
    argPoints = arguments[0];
  }

  if (!Array.isArray(argPoints[0])) {
    // Assume a flat list of coordinates to convert into pairs.
    points = [];

    for (let p = 0; p < argPoints.length; p += 2) {
      points.push([argPoints[p], argPoints[p + 1]]);
    }
  } else {
    points = argPoints; // Assume array of pairs was passed.
  }

  if (filled) {
    // 🚀 MASSIVE OPTIMIZATION: Use fast fillTri for triangles instead of slow fillShape
    if (points.length === 3) {
      // Triangle - use the optimized fillTri function
      const triPixels = [];
      fillTri(points, triPixels);
      triPixels.forEach((p) => point(p));
    } else {
      // For other polygons, fall back to the general fillShape algorithm
      fillShape(points);
    }
  } else {
    // Make lines from 1->2->3->...->1
    if (thickness === 1) {
      points.forEach((p, i) => {
        const lastPoint = i < points.length - 1 ? points[i + 1] : points[0];
        line(...p, ...lastPoint);
      });
    } else {
      // Thicker outline using pline.
      points.push(points[0]);
      pline(
        points.map((p) => p2.of(...p)),
        thickness,
      );
    }
  }
  
  graphPerf.track('shape', performance.now() - shapeStart); // 🚨 PERFORMANCE TRACKING
}

// Note: This may not be very fast. It was written by ChatGPT. 23.02.11.12.51
// Note: The most efficient algorithm in C I could find as an alternate:
//       https://gist.github.com/ideasman42/983738130f754ef58ffa66bcdbbab892
// Fills a shape using the scan line algorithm, hitting every pixel via `point`.
function fillShape(points) {
  // Find the minimum and maximum y-coordinates of the points
  let minY = Infinity;
  let maxY = -Infinity;
  for (let i = 0; i < points.length; i++) {
    minY = min(minY, points[i][1]);
    maxY = max(maxY, points[i][1]);
  }

  // For each scan line from minY to maxY
  for (let y = minY; y <= maxY; y++) {
    // Find the intersections of the scan line with the edges of the polygon
    let intersections = [];
    for (let i = 0; i < points.length; i++) {
      let p1 = points[i];
      let p2 = points[(i + 1) % points.length];
      if ((p1[1] <= y && y < p2[1]) || (p2[1] <= y && y < p1[1])) {
        let x = ((y - p1[1]) * (p2[0] - p1[0])) / (p2[1] - p1[1]) + p1[0];
        intersections.push(x);
      }
    }

    // Sort the intersections in increasing order
    intersections.sort((a, b) => a - b);

    // Fill in the pixels between each pair of intersections
    for (let i = 0; i < intersections.length; i += 2) {
      let x1 = floor(intersections[i]);
      let x2 = ceil(intersections[i + 1]);
      for (let x = x1; x < x2; x++) {
        point(x, y);
      }
    }
  }
}

// Draws a triangle from three points, with optional fill mode.
// Usage: tri(x1, y1, x2, y2, x3, y3, mode = "fill")
// mode can be "fill", "outline", "out", or "inline", "in"
function tri() {
  let x1, y1, x2, y2, x3, y3;
  let mode = "fill";

  // Apply the TODO: If any argument is NaN then just make it 'undefined'
  for (let i = 0; i < arguments.length; i++) {
    if (Number.isNaN(arguments[i])) {
      arguments[i] = undefined;
    }
  }

  if (arguments.length === 6) {
    // tri(x1, y1, x2, y2, x3, y3)
    x1 = arguments[0];
    y1 = arguments[1];
    x2 = arguments[2];
    y2 = arguments[3];
    x3 = arguments[4];
    y3 = arguments[5];
  } else if (arguments.length === 7) {
    // tri(x1, y1, x2, y2, x3, y3, mode)
    x1 = arguments[0];
    y1 = arguments[1];
    x2 = arguments[2];
    y2 = arguments[3];
    x3 = arguments[4];
    y3 = arguments[5];
    mode = arguments[6];
  } else if (arguments.length === 1) {
    // tri([x1, y1, x2, y2, x3, y3]) or tri({points: [...], mode: "..."})
    if (Array.isArray(arguments[0])) {
      const coords = arguments[0];
      if (coords.length >= 6) {
        x1 = coords[0];
        y1 = coords[1];
        x2 = coords[2];
        y2 = coords[3];
        x3 = coords[4];
        y3 = coords[5];
      }
    } else if (arguments[0] && arguments[0].points) {
      const obj = arguments[0];
      const coords = obj.points;
      if (Array.isArray(coords) && coords.length >= 6) {
        x1 = coords[0];
        y1 = coords[1];
        x2 = coords[2];
        y2 = coords[3];
        x3 = coords[4];
        y3 = coords[5];
      }
      if (obj.mode) mode = obj.mode;
    }
  } else if (arguments.length === 2) {
    // tri([x1, y1, x2, y2, x3, y3], mode)
    if (Array.isArray(arguments[0])) {
      const coords = arguments[0];
      if (coords.length >= 6) {
        x1 = coords[0];
        y1 = coords[1];
        x2 = coords[2];
        y2 = coords[3];
        x3 = coords[4];
        y3 = coords[5];
      }
      mode = arguments[1];
    }
  } else {
    return console.error("Invalid triangle call. Expected tri(x1, y1, x2, y2, x3, y3) or tri(x1, y1, x2, y2, x3, y3, mode)");
  }

  // Validate coordinates
  if ([x1, y1, x2, y2, x3, y3].some(coord => coord === undefined || isNaN(coord))) {
    return console.error("Invalid triangle coordinates:", { x1, y1, x2, y2, x3, y3 });
  }

  // Random parameters if undefined, null, or NaN.
  if (nonvalue(x1)) x1 = randInt(width);
  if (nonvalue(y1)) y1 = randInt(height);
  if (nonvalue(x2)) x2 = randInt(width);
  if (nonvalue(y2)) y2 = randInt(height);
  if (nonvalue(x3)) x3 = randInt(width);
  if (nonvalue(y3)) y3 = randInt(height);

  // Floor the coordinates
  x1 = floor(x1);
  y1 = floor(y1);
  x2 = floor(x2);
  y2 = floor(y2);
  x3 = floor(x3);
  y3 = floor(y3);

  if (mode === "fill" || mode === "") {
    // Use the existing fillShape function
    const points = [[x1, y1], [x2, y2], [x3, y3]];
    fillShape(points);
  } else {
    // Draw outline with specified mode
    const thickness = parseInt(mode.split(":")[1]) || 1;
    const outlineMode = mode.split(":")[0];
    
    if (outlineMode === "outline" || outlineMode === "out") {
      // Draw three lines forming the triangle outline
      line(x1, y1, x2, y2);
      line(x2, y2, x3, y3);
      line(x3, y3, x1, y1);
    } else if (outlineMode === "inline" || outlineMode === "in") {
      // For inline mode, we would need to shrink the triangle inward
      // For now, just draw the outline (this could be enhanced later)
      line(x1, y1, x2, y2);
      line(x2, y2, x3, y3);
      line(x3, y3, x1, y1);
    } else {
      // Default to fill if mode is unrecognized
      const points = [[x1, y1], [x2, y2], [x3, y3]];
      fillShape(points);
    }
  }
}

// Renders a square grid at x, y given cols, rows, and scale.
// Buffer is optional, and if present will render the pixels at scale starting
// from the top left corner of the buffer, repeating if needed to fill the grid.

function grid(
  {
    box: { x, y, w: cols, h: rows },
    transform: { scale, angle, width: twidth, height: theight, anchor },
    centers = [],
  },
  buffer,
) {
  const gridStart = performance.now(); // 🚨 PERFORMANCE TRACKING
  
  // 🚀 PERFORMANCE OPTIMIZATION: Use lower precision when running slowly
  const isSlowFrame = graphPerf && graphPerf.lastFPS && graphPerf.lastFPS < 15;
  
  const oc = c.slice(); // Remember the original color.

  let w, h;

  // Note: `transform` is a bit overloaded here and looks for the presence
  //        of certain properties to determine the behavior.
  //        See `function paste` here in `graph` for an example usage.
  if (scale !== undefined) {
    if (number(scale)) scale = { x: scale, y: scale };
    w = cols * scale.x;
    h = rows * scale.y;

    //       ❤️‍🔥
    // TODO: Allow for scaleX and scaleY values on transform / let scale
    //       be an input array.
  } else if (twidth !== undefined && theight !== undefined) {
    w = twidth;
    h = theight;
    scale = { x: w / cols, y: h / rows };
    //      ^ Give scale a separate width and height.
  }

  const colPix = w / cols,
    rowPix = h / rows;

  if (scale.x < 0) x -= w + 1;
  if (scale.y < 0) y -= h + 1;

  angle = wrap(angle, 360); // Keep angle positive.

  // Always make sure we are at the mid-point of the pixel we rotate around.
  // given the image resolution's even / oddness on each axis.
  // (Make some off by 1 adjustments for specific angles.)

  let xmod = 0,
    ymod = 0;

  if (angle) {
    // Odd width.
    if (w % 2 !== 0 && h % 2 === 0) {
      if (x % 1 !== 0 && angle === 90) xmod += 0.5;

      // if (angle === 90 || angle === 270) ymod += 1;
      if (angle === 270) {
        xmod += x % 1 !== 0 ? 1.0 : 0.5;
        ymod += 0.5;
      }

      if (angle === 180) {
        ymod += 0.5;
        if (x % 1 === 0) xmod += 0.5;
      }
    }
    if (h % 2 !== 0 && w % 2 === 0) {
      // Odd height.
      if (y % 1 !== 0 && angle === 90) ymod += 0.5;

      if (angle === 270) {
        xmod += 0.5;
        ymod += y % 1 !== 0 ? 1.0 : 0.5;
      }

      if (angle === 180) {
        xmod += 0.5;
        if (y % 1 === 0) ymod += 0.5;
      }
    } else if (w % 2 === 0 && h % 2 === 0) {
      // Both even...
      xmod += 0.5;
      ymod += 0.5;
    } else if (w % 2 !== 0 && h % 2 !== 0) {
      // Both odd...
      xmod += 0.5;
      ymod += 0.5;
    }
  }

  // Make off by 1 adjustments for specific scale inverstions.
  // (This is kind of hacky. 23.07.20.13.44)
  if (scale.x < 0 && scale.y > 0) {
    if (angle >= 90 && angle < 270) ymod += 1 * sign(-scale.y);
    if (angle >= 180 && angle <= 270) xmod += 1 * sign(-scale.x);
  } else if (scale.x > 0 && scale.y > 0) {
    if (angle >= 90 && angle < 270) xmod += 1 * sign(-scale.x);
    if (angle >= 180 && angle <= 270) ymod += 1 * sign(-scale.y);
  } else if (scale.y < 0 && scale.x > 0) {
    if (angle >= 90 && angle < 270) ymod += 1 * sign(-scale.y);
    if (angle >= 180 && angle <= 270) xmod += 1 * sign(-scale.x);
  } else if (scale.y < 0 && scale.x < 0) {
    if (angle >= 90 && angle < 270) xmod += 1 * sign(-scale.x);
    if (angle >= 180 && angle <= 270) ymod += 1 * sign(-scale.y);
  }

  x += xmod;
  y += ymod;

  // Rotate around the anchor, or center.
  let centerX, centerY;
  if (anchor) {
    centerX = anchor.x + x;
    centerY = anchor.y + y;
  } else {
    centerX = x + w / 2;
    centerY = y + h / 2;
  }

  angle = radians(angle); // Sets angle to 0 if it was undefined.
  // Draw a scaled image if the buffer is present.
  if (buffer) {
    const bufWidth = buffer.width;
    const bufHeight = buffer.height;
    const bufPixels = buffer.pixels;

    // Use fast integer conversions and pre-calculate values
    const scaleXAbs = ~~abs(scale.x); // Fast float-to-int conversion
    const scaleYAbs = ~~abs(scale.y);
    const isAngleZero = angle === 0;

    // Pre-calculate trigonometric values only if needed
    let cosValue, sinValue;
    if (!isAngleZero) {
      cosValue = cos(angle);
      sinValue = sin(angle);
    }

    // Pre-calculate integer boundaries and scales
    const bufferWidth = scaleXAbs;
    const bufferHeight = scaleYAbs;
    const halfBoxWidth = bufferWidth >> 1; // Fast division by 2
    const halfBoxHeight = bufferHeight >> 1;
    const adjustedBufferWidth = bufferWidth + (halfBoxWidth << 1); // Fast multiplication by 2
    const adjustedBufferHeight = bufferHeight + (halfBoxHeight << 1);

    // Pre-calculate row and column pixel increments
    const colPixInt = ~~(w / cols);
    const rowPixInt = ~~(h / rows);

    // Fast path for simple scaling (no rotation, integer scales)
    if (
      isAngleZero &&
      scale.x === scaleXAbs &&
      scale.y === scaleYAbs &&
      scale.x > 0 &&
      scale.y > 0
    ) {
      // Ultra-fast nearest-neighbor scaling with direct buffer operations
      for (let j = 0; j < rows; j += 1) {
        const srcY = j % bufHeight;
        const destStartY = ~~(y + j * rowPixInt);
        const destEndY = ~~(y + (j + 1) * rowPixInt);
        const pixelHeight = destEndY - destStartY;

        if (pixelHeight > 0 && destStartY >= 0 && destEndY <= height) {
          for (let i = 0; i < cols; i += 1) {
            const srcX = i % bufWidth;            const srcIndex = (srcX + srcY * bufWidth) << 2; // Fast * 4
            if (srcIndex < bufPixels.length) {
              // Extract color data directly
              const r = bufPixels[srcIndex];
              const g = bufPixels[srcIndex + 1];
              const b = bufPixels[srcIndex + 2];
              const a = bufPixels[srcIndex + 3];

              const destStartX = ~~(x + i * colPixInt);
              const destEndX = ~~(x + (i + 1) * colPixInt);
              const pixelWidth = destEndX - destStartX;

              if (pixelWidth > 0 && destStartX >= 0 && destEndX <= width) {
                // Set color once and draw block efficiently
                color(r, g, b, a);

                // Use box for rectangular fills when possible (faster than multiple lineh calls)
                if (pixelWidth > 1 && pixelHeight > 1) {
                  box(destStartX, destStartY, pixelWidth, pixelHeight, "fill");
                } else if (pixelHeight === 1) {
                  // Single horizontal line
                  lineh(destStartX, destEndX - 1, destStartY);
                } else {
                  // Vertical line or single pixel
                  for (let dy = 0; dy < pixelHeight; dy += 1) {
                    lineh(destStartX, destEndX - 1, destStartY + dy);
                  }
                }
              }
            }
          }
        }
      }
    } else {
      // Complex path for rotation and non-integer scaling (preserve existing functionality)
      // 🚀 PERFORMANCE OPTIMIZATION: Reduce resolution when running slowly
      const skipStep = isSlowFrame ? 2 : 1; // Skip every other pixel when slow
      
      for (let j = 0; j < rows; j += skipStep) {
        const plotY = y + rowPix * j;
        const repeatY = j % bufHeight;

        for (let i = 0; i < cols; i += skipStep) {
          const plotX = x + colPix * i;

          let finalX, finalY;

          if (isAngleZero) {
            finalX = plotX;
            finalY = plotY;
          } else {
            // Rotate the plot coordinates around the center of the grid
            const dx = plotX - centerX;
            const dy = plotY - centerY;
            finalX = dx * cosValue - dy * sinValue + centerX;
            finalY = dx * sinValue + dy * cosValue + centerY;
          }

          // Adjusted boundary checks
          if (
            finalX < -adjustedBufferWidth ||
            finalX > width + adjustedBufferWidth ||
            finalY < -adjustedBufferHeight ||
            finalY > height + adjustedBufferHeight
          ) {
            continue; // Skip drawing this box
          }

          // Find the proper color
          const repeatX = i % bufWidth;
          const pixIndex = (repeatX + bufWidth * repeatY) << 2; // Fast multiplication by 4

          if (pixIndex < bufPixels.length) {
            const colorData = [
              bufPixels[pixIndex],
              bufPixels[pixIndex + 1],
              bufPixels[pixIndex + 2],
              bufPixels[pixIndex + 3],
            ];
            color(...colorData);

            // Calculate destination pixel ranges ensuring no gaps
            const scaleX = abs(scale.x);
            const scaleY = abs(scale.y);

            const destStartX = ~~(x + i * scaleX); // Fast floor conversion
            const destEndX = ~~(x + (i + 1) * scaleX);
            const destStartY = ~~(y + j * scaleY);
            const destEndY = ~~(y + (j + 1) * scaleY);

            const pixelWidth = destEndX - destStartX;
            const pixelHeight = destEndY - destStartY;

            // Only draw if there's actually area to fill
            if (pixelWidth > 0 && pixelHeight > 0) {
              // Apply rotation if needed
              if (isAngleZero) {
                // Optimized no-rotation path
                if (pixelWidth > 1 && pixelHeight > 1) {
                  // Use box for efficiency when drawing rectangles
                  box(destStartX, destStartY, pixelWidth, pixelHeight, "fill");
                } else {
                  // Fall back to line drawing for thin regions
                  for (let dy = 0; dy < pixelHeight; dy += 1) {
                    lineh(destStartX, destEndX - 1, destStartY + dy);
                  }
                }
              } else {
                // Rotation path - preserve existing pixel-by-pixel approach for accuracy
                for (let dy = 0; dy < pixelHeight; dy += 1) {
                  for (let dx = 0; dx < pixelWidth; dx += 1) {
                    const px = destStartX + dx;
                    const py = destStartY + dy;

                    // Rotate around center
                    const relX = px - centerX;
                    const relY = py - centerY;
                    const rotX = relX * cosValue - relY * sinValue + centerX;
                    const rotY = relX * sinValue + relY * cosValue + centerY;

                    plot(~~rotX, ~~rotY); // Fast floor conversion
                  }
                }
              }
            }
          }
        }
      }
    }
  } else {
    // Draw a debug / blueprint grid if no buffer is present.

    // Plot a point in each of the four corners.
    const right = x + w - 1,
      bottom = y + h - 1;

    color(64, 64, 64);
    plot(x, y);
    plot(right, y);
    plot(x, bottom);
    plot(right, bottom);
    color(...oc);

    // Draw each grid square, with optional center points.
    for (let i = 0; i < cols; i += 1) {
      const plotX = x + colPix * i;
      for (let j = 0; j < rows; j += 1) {
        const plotY = y + rowPix * j;

        // Lightly shade this grid square, alternating tint on evens and odds.
        const alphaMod = oc[3] / 255;
        color(oc[0], oc[1], oc[2], even(i + j) ? 50 * alphaMod : 75 * alphaMod);
        box(plotX, plotY, scale.x, scale.y);

        // Color in the centers of each grid square.
        centers.forEach((p) => {
          color(oc[0], oc[1], oc[2], 100);
          plot(plotX + p.x, plotY + p.y);
        });
      }
    }

    color(...oc); // Restore color.
  }
  
  // 🚨 PERFORMANCE TRACKING
  const gridEnd = performance.now();
  graphPerf.track('grid', gridEnd - gridStart);
}

// Rendering stored drawings.
// Silently fails if `drawing` is left `undefined`.
// Params:
//   drawing, x, y, scale = 1, angle = 0, thickness = 1
//   drawing, position, scale = 1, angle = 0, thickness = 1
function draw() {
  const args = arguments;
  let drawing = args[0],
    x,
    y,
    scale = 1,
    angle = 0,
    thickness = 1;
  if (typeof args[1] === "number") {
    x = args[1];
    y = args[2];
    scale = args[3] || scale;
    angle = args[4] || angle;
    thickness = args[5] || thickness;
  } else if (typeof args[1] === "object") {
    drawing = args[0];
    if (Array.isArray(args[1])) {
      x = args[1][0];
      y = args[1][1];
    } else {
      x = args[1].x;
      y = args[1].y;
    }
    scale = args[2] || scale;
    angle = args[3] || angle;
    thickness = args[4] || thickness;
  }

  if (drawing === undefined) return;

  // Apply BDF offset if present (for proper baseline positioning)
  if (drawing.offset) {
    x += drawing.offset[0] * scale; // xOffset
    
    // Use baseline-corrected offset if available, otherwise fall back to original
    const yOffset = drawing.baselineOffset ? drawing.baselineOffset[1] : drawing.offset[1];
    y += yOffset * scale; // Use baseline-relative positioning
  }

  // TODO: Eventually make this the call: rotatePoint(args[0], args[1], 0, 0);
  angle = radians(angle);
  const s = sin(angle);
  const c = cos(angle);

  x = floor(x);
  y = floor(y);

  pan(x, y);

  // 🍎 Build a poly line out of subsequent points sharing start & end points.
  // 🧠 And plot other commands.
  const gesture = []; // Keep track of continuous lines.

  function paintGesture() {
    // Draw each gesture path and then kill it.
    thickness === 1 ? poly(gesture) : pline(gesture, thickness);
    gesture.length = 0;
  }

  drawing?.commands?.forEach(({ name, args }, i) => {
    args = args.map((a) => a * scale);

    // console.log(name, i, drawing.commands.length, drawing);

    if (name === "line") {
      let x1 = args[0]; // x1
      let y1 = args[1]; // y1
      let x2 = args[2]; // x2
      let y2 = args[3]; // y2
      let nx1 = x1 * c - y1 * s;
      let ny1 = x1 * s + y1 * c;
      let nx2 = x2 * c - y2 * s;
      let ny2 = x2 * s + y2 * c;

      if (nx1 !== nx2 || ny1 !== ny2) {
        if (thickness === 1) {
          gesture.push([nx1, ny1], [nx2, ny2]);
        } else {
          gesture.push({ x: nx1, y: ny1 }, { x: nx2, y: ny2 });
        }
      }

      const nextCommand = drawing.commands[i + 1];
      if (nextCommand && nextCommand.name === "line") {
        const nextArgs = nextCommand.args.map((a) => a * scale);
        if (args[2] !== nextArgs[0] || args[3] !== nextArgs[1]) {
          // If the last point of cur line and 1st point of next line are diff.
          // Then we can paint!
          paintGesture();
        } else {
          // Otherwise we should pop off the last point to avoid repeats.
          gesture.pop();
        }
      } else {
        paintGesture();
      }
    } else if (name === "point") {
      thickness === 1
        ? point(...args)
        : circle(args[0], args[1], thickness / 2, true);
    }
  });

  pan(-x, -y);
}

// Write out a line of text.
// TODO: Add directionality using a bresenham algorithm.
//       - Must know about height.
// TODO: Abstract this to another level, similar to 'draw' above.
//       - I would need to get the final drawing API and pass that to
//         a module that builds on it, then also has functions that
//         get added back to it. This would be *graph: layer 2*.
function printLine(
  text,
  font,
  startX,
  startY,
  blockWidth = 6,
  scale = 1,
  xOffset = 0,
  thickness = 1,
  rotation = 0,
  fontMetadata = null,
) {
  if (!text) return;

  // Early culling for mask bounds
  if (activeMask) {
    const lineHeight = font?.A?.box?.height || 10; // Estimate line height
    const scaledHeight = lineHeight * scale;

    // Check if the entire line is outside the mask bounds
    if (
      startY >= activeMask.y + activeMask.height ||
      startY + scaledHeight <= activeMask.y ||
      startX >= activeMask.x + activeMask.width ||
      startX + text.length * blockWidth * scale <= activeMask.x
    ) {
      return; // Don't render if completely outside mask
    }
  }

  // Check if font supports proportional spacing using passed metadata instead of glyph object
  // This avoids triggering the BDF proxy when checking font properties
  const isProportional = fontMetadata?.proportional === true || 
                         fontMetadata?.bdfFont === "MatrixChunky8" ||
                         fontMetadata?.name === "MatrixChunky8";

  if (isProportional) {
    // Use character advance widths from font metadata
    const advances = fontMetadata?.advances || {};
    const bdfOverrides = fontMetadata?.bdfOverrides || {};

    // Calculate character positions for proportional spacing
    let currentX = startX + xOffset;
    
    [...text.toString()].forEach((char, i) => {
      const charX = currentX;
      let charY = startY;
      
      // Apply BDF overrides if they exist for this character
      if (bdfOverrides[char]) {
        const override = bdfOverrides[char];
        if (override.x !== undefined) {
          charX += override.x * scale;
        }
        if (override.y !== undefined) {
          charY += override.y * scale;
        }
      }
      
      // Get character advance width from font data, fallback to blockWidth
      const charAdvance = advances[char] || blockWidth;
      
      draw(
        font[char],
        charX,
        charY,
        scale,
        rotation,
        thickness,
      );
      
      // Advance to next character position
      currentX += charAdvance * scale;
    });
  } else {
    // Use original monospace logic for fonts without proportional flag
    [...text.toString()].forEach((char, i) => {
      draw(
        font[char],
        startX + blockWidth * scale * i + xOffset,
        startY,
        scale,
        rotation,
        thickness,
      );
    });
  }
}

function noise16() {
  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = Math.max(0, activeMask.x);
    minY = Math.max(0, activeMask.y);
    maxX = Math.min(width, activeMask.x + activeMask.width);
    maxY = Math.min(height, activeMask.y + activeMask.height);
  }

  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const i = (y * width + x) * 4;
      pixels[i] = byteInterval17(randInt(16)); // r
      pixels[i + 1] = byteInterval17(randInt(16)); // g
      pixels[i + 2] = byteInterval17(randInt(16)); // b
      pixels[i + 3] = 255; // a
    }
  }
}

function noise16DIGITPAIN() {
  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = Math.max(0, activeMask.x);
    minY = Math.max(0, activeMask.y);
    maxX = Math.min(width, activeMask.x + activeMask.width);
    maxY = Math.min(height, activeMask.y + activeMask.height);
  }

  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const i = (y * width + x) * 4;
      pixels[i] = byteInterval17(randInt(16)) * 0.6; // r
      pixels[i + 1] = byteInterval17(randInt(16)) * 0.15; // g
      pixels[i + 2] = byteInterval17(randInt(16)) * 0.55; // b
      pixels[i + 3] = 255; // a
    }
  }
}

function noise16Aesthetic() {
  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = Math.max(0, activeMask.x);
    minY = Math.max(0, activeMask.y);
    maxX = Math.min(width, activeMask.x + activeMask.width);
    maxY = Math.min(height, activeMask.y + activeMask.height);
  }

  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const i = (y * width + x) * 4;
      pixels[i] = byteInterval17(randInt(16)) * 0.4; // r
      pixels[i + 1] = byteInterval17(randInt(16)) * 0.15; // g
      pixels[i + 2] = byteInterval17(randInt(16)) * 0.8; // b
      pixels[i + 3] = 255; // a
    }
  }
}

function noise16Sotce() {
  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Don't apply pan translation to mask bounds - mask is already set at current pan position
    minX = Math.max(0, activeMask.x);
    minY = Math.max(0, activeMask.y);
    maxX = Math.min(width, activeMask.x + activeMask.width);
    maxY = Math.min(height, activeMask.y + activeMask.height);
  }

  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const i = (y * width + x) * 4;
      if (flip()) pixels[i] = byteInterval17(14 + randInt(2)); // r
      if (flip()) pixels[i + 1] = byteInterval17(8 + randInt(2)) * 0.9; // g
      if (flip()) pixels[i + 2] = byteInterval17(8 + randInt(2)) * 0.9; // b
      pixels[i + 3] = 255; // a
    }
  }
}

function noiseTinted(tint, amount, saturation) {
  // console.log("Tinting:", tint, amount, saturation);
  tint = findColor(tint);
  for (let i = 0; i < pixels.length; i += 4) {
    const grayscale = randInt(255);
    pixels[i] = lerp(
      lerp(grayscale, randInt(255), saturation),
      tint[0],
      amount,
    ); // r
    pixels[i + 1] = lerp(
      lerp(grayscale, randInt(255), saturation),
      tint[1],
      amount,
    ); // g
    pixels[i + 2] = lerp(
      lerp(grayscale, randInt(255), saturation),
      tint[2],
      amount,
    ); // b
    pixels[i + 3] = 255; // a
  }
}

// Accumulated fractional steps for smooth fractional spinning
let spinAccumulator = 0;

// Accumulated fractional zoom for smooth zooming (additive like scroll/spin)
let zoomAccumulator = 0.0;
let zoomTimeOffset = Math.random() * 1000; // Random time offset for organic variation

// Accumulated fractional scroll for smooth scrolling
let scrollAccumulatorX = 0;
let scrollAccumulatorY = 0;

// Reset scroll accumulators - called when pieces change
function resetScrollState() {
  scrollAccumulatorX = 0;
  scrollAccumulatorY = 0;
}

// Accumulated fractional shear for smooth shearing
let shearAccumulatorX = 0;
let shearAccumulatorY = 0;

// Accumulated radial displacement for smooth sucking
let suckAccumulator = 0.0;

// Per-pixel shear accumulation for ensuring all pixels eventually move
let pixelShearAccumX = null;
let pixelShearAccumY = null;

// Scroll the entire pixel buffer by x and/or y pixels with wrapping
function scroll(dx = 0, dy = 0) {
  if (dx === 0 && dy === 0) return; // No change needed

  // Performance optimization: skip expensive scroll operations during startup
  // Check if we're in early frames by looking at global frame counter
  if (typeof globalThis !== 'undefined' && globalThis.$api?.paintCount) {
    const frameCount = Number(globalThis.$api.paintCount);
    if (frameCount < 10) {
      // During startup, just accumulate the scroll values but don't apply them
      scrollAccumulatorX += dx;
      scrollAccumulatorY += dy;
      return;
    }
  }

  // Accumulate fractional scroll amounts
  scrollAccumulatorX += dx;
  scrollAccumulatorY += dy;

  // Extract integer parts for actual scrolling
  const integerDx = Math.floor(scrollAccumulatorX);
  const integerDy = Math.floor(scrollAccumulatorY);

  // Keep fractional remainders
  scrollAccumulatorX -= integerDx;
  scrollAccumulatorY -= integerDy;

  // Only proceed if we have integer pixels to scroll
  if (integerDx === 0 && integerDy === 0) {
    return;
  }

  // Determine the area to scroll (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Apply pan translation to mask bounds and ensure they're within screen bounds
    minX = Math.max(0, Math.min(width, activeMask.x + panTranslation.x));
    maxX = Math.max(
      0,
      Math.min(width, activeMask.x + activeMask.width + panTranslation.x),
    );
    minY = Math.max(0, Math.min(height, activeMask.y + panTranslation.y));
    maxY = Math.max(
      0,
      Math.min(height, activeMask.y + activeMask.height + panTranslation.y),
    );
  }

  const boundsWidth = maxX - minX;
  const boundsHeight = maxY - minY;

  // Early exit if bounds are invalid
  if (boundsWidth <= 0 || boundsHeight <= 0) {
    return;
  }

  // Use integer scroll amounts
  let finalDx = ((integerDx % boundsWidth) + boundsWidth) % boundsWidth;
  let finalDy = ((integerDy % boundsHeight) + boundsHeight) % boundsHeight;

  if (finalDx === 0 && finalDy === 0) {
    return; // No effective shift after normalization
  }

  // Create a complete copy of the working area for safe reading
  // 🛡️ SAFETY CHECK: If pixels buffer is detached, recreate it
  if (pixels.buffer && pixels.buffer.detached) {
    console.warn('🚨 Pixels buffer detached in scroll, recreating from screen dimensions');
    pixels = new Uint8ClampedArray(width * height * 4);
    pixels.fill(0); // Fill with transparent black
  }
  
  const tempPixels = new Uint8ClampedArray(pixels);

  // General case: pixel-by-pixel with proper bounds checking
  for (let y = 0; y < boundsHeight; y++) {
    for (let x = 0; x < boundsWidth; x++) {
      // Calculate source coordinates with wrapping within bounds
      const srcX = minX + ((x + boundsWidth - finalDx) % boundsWidth);
      const srcY = minY + ((y + boundsHeight - finalDy) % boundsHeight);

      // Calculate destination coordinates
      const destX = minX + x;
      const destY = minY + y;

      // Ensure coordinates are within valid bounds
      if (
        srcX >= minX &&
        srcX < maxX &&
        srcY >= minY &&
        srcY < maxY &&
        destX >= minX &&
        destX < maxX &&
        destY >= minY &&
        destY < maxY
      ) {
        const srcOffset = (srcY * width + srcX) * 4;
        const destOffset = (destY * width + destX) * 4;

        // Copy RGBA values
        pixels[destOffset] = tempPixels[srcOffset];
        pixels[destOffset + 1] = tempPixels[srcOffset + 1];
        pixels[destOffset + 2] = tempPixels[srcOffset + 2];
        pixels[destOffset + 3] = tempPixels[srcOffset + 3];
      }
    }
  }
}

// Rotates pixels in concentric rings around a specified anchor point
// steps: positive for clockwise, negative for counterclockwise
// anchorX, anchorY: optional anchor point (defaults to center of working area)
// 🚀 Pre-allocated buffers for performance
let spinBuffer;
let zoomBuffer;

// 🧪 EXPERIMENTAL: Block-based processing for better performance on large images
let useBlockProcessing = true; // Can be toggled at runtime
const BLOCK_SIZE = 64; // Process in 64x64 blocks

let spinSkipCounter = 0;

// 🧪 EXPERIMENTAL: Toggle block-based processing for performance testing
function setBlockProcessing(enabled) {
  useBlockProcessing = enabled;
  console.log(`🧪 Block processing ${enabled ? 'ENABLED' : 'DISABLED'} (${BLOCK_SIZE}x${BLOCK_SIZE} blocks)`);
}

// Each ring rotates by exactly 'steps' pixels, preserving all data
function spin(steps = 0, anchorX = null, anchorY = null) {
  const spinStart = performance.now();
  
  if (steps === 0) {
    graphPerf.track("spin", 0);
    return;
  }

  // 🚀 DEFAULT: Use block-based processing for better performance (26% faster)
  return spinBlockBased(steps, anchorX, anchorY, spinStart);
}

// 🚀 SIMD Helper: Process individual pixel with wrapping and sampling
function processSpinPixel(srcX, srcY, destIdx, minX, minY, maxXMinus1, maxYMinus1, workingWidth, workingHeight) {
  // Fast wrapping (same logic as regular spin)
  let wrappedSrcX = srcX;
  let wrappedSrcY = srcY;
  
  if (srcX < minX || srcX >= minX + workingWidth) {
    const normalizedX = srcX - minX;
    wrappedSrcX = minX + (normalizedX - workingWidth * Math.floor(normalizedX / workingWidth));
    if (wrappedSrcX < minX) wrappedSrcX += workingWidth;
  }
  if (srcY < minY || srcY >= minY + workingHeight) {
    const normalizedY = srcY - minY;
    wrappedSrcY = minY + (normalizedY - workingHeight * Math.floor(normalizedY / workingHeight));
    if (wrappedSrcY < minY) wrappedSrcY += workingHeight;
  }
  
  // Fast nearest neighbor with bounds clamping
  const nearestSrcX = Math.max(minX, Math.min(maxXMinus1, Math.round(wrappedSrcX)));
  const nearestSrcY = Math.max(minY, Math.min(maxYMinus1, Math.round(wrappedSrcY)));
  
  // Direct array access with pre-calculated offsets
  const srcIdx = (nearestSrcY * width + nearestSrcX) * 4;
  
  // Unrolled RGBA copy for maximum speed
  pixels[destIdx] = spinBuffer[srcIdx];
  pixels[destIdx + 1] = spinBuffer[srcIdx + 1];
  pixels[destIdx + 2] = spinBuffer[srcIdx + 2];
  pixels[destIdx + 3] = spinBuffer[srcIdx + 3];
}

// Static buffers for SIMD spin function
spin.coordBuffers = null;
spin.destIndices = null;

// 🚀 EXPERIMENTAL: SIMD-optimized spin processing using typed arrays
function spinSimd(steps, anchorX, anchorY, spinStart) {
  // Same early exits as regular spin
  if (graphPerf && graphPerf.lastFPS && graphPerf.lastFPS < 6) {
    spinSkipCounter++;
    if (spinSkipCounter % 2 !== 0) {
      graphPerf.track('spin', 0);
      return;
    }
  } else {
    spinSkipCounter = 0;
  }

  if (Math.abs(steps) < 0.5) {
    graphPerf.track('spin', 0);
    return;
  }

  // Handle fractional steps
  spinAccumulator += steps;
  const integerSteps = floor(spinAccumulator);
  spinAccumulator -= integerSteps;
  if (integerSteps === 0) {
    graphPerf.track('spin', 0);
    return;
  }

  // Determine the area to process
  let minX = 0, minY = 0, maxX = width, maxY = height;
  if (activeMask) {
    minX = activeMask.x + panTranslation.x;
    minY = activeMask.y + panTranslation.y;
    maxX = activeMask.x + activeMask.width + panTranslation.x;
    maxY = activeMask.y + activeMask.height + panTranslation.y;
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;
  const centerX = anchorX !== null ? anchorX : minX + floor(workingWidth / 2);
  const centerY = anchorY !== null ? anchorY : minY + floor(workingHeight / 2);

  // 🛡️ SAFETY CHECK: Buffer management
  if (pixels.buffer && pixels.buffer.detached) {
    console.warn('🚨 Pixels buffer detached in spinSimd, recreating');
    pixels = new Uint8ClampedArray(width * height * 4);
    pixels.fill(0);
  }
  
  const bufferSize = width * height * 4;
  if (!spinBuffer || spinBuffer.length !== bufferSize) {
    spinBuffer = new Uint8ClampedArray(bufferSize);
  }
  
  if (pixels.length !== bufferSize) {
    pixels = new Uint8ClampedArray(bufferSize);
    pixels.fill(0);
  }
  
  spinBuffer.set(pixels);

  // 🚀 SIMD: Create Float32Array views for vectorized coordinate math
  const coordCount = (maxY - minY) * (maxX - minX);
  const maxCoords = Math.ceil(coordCount / 4) * 4; // Round up to multiple of 4
  
  // Reuse coordinate buffers to avoid allocation overhead
  if (!spinSimd.coordBuffers || spinSimd.coordBuffers.length < maxCoords * 2) {
    spinSimd.coordBuffers = new Float32Array(maxCoords * 2); // x,y pairs
    spinSimd.srcIndices = new Uint32Array(maxCoords);
    spinSimd.destIndices = new Uint32Array(maxCoords);
  }
  
  const coords = spinSimd.coordBuffers;
  const srcIndices = spinSimd.srcIndices;
  const destIndices = spinSimd.destIndices;

  // Pre-calculate constants for SIMD operations
  const twoPi = 2 * PI;
  const maxXMinus1 = maxX - 1;
  const maxYMinus1 = maxY - 1;
  
  let coordIndex = 0;
  
  // 🚀 SIMD: Generate all coordinates in vectorized batches
  for (let destY = minY; destY < maxY; destY++) {
    const dy = destY - centerY;
    const dy2 = dy * dy;
    const destRowOffset = destY * width;
    
    for (let destX = minX; destX < maxX; destX++) {
      const dx = destX - centerX;
      const distanceSquared = dx * dx + dy2;
      
      // Fast path for center pixels (same as regular spin)
      if (distanceSquared < 1.0) {
        const idx = (destRowOffset + destX) * 4;
        pixels[idx] = spinBuffer[idx];
        pixels[idx + 1] = spinBuffer[idx + 1];
        pixels[idx + 2] = spinBuffer[idx + 2];
        pixels[idx + 3] = spinBuffer[idx + 3];
        continue;
      }
      
      // Skip very distant pixels
      if (distanceSquared > 16000000) continue;
      
      // Store coordinates for SIMD processing
      coords[coordIndex * 2] = dx;
      coords[coordIndex * 2 + 1] = dy;
      destIndices[coordIndex] = (destRowOffset + destX) * 4;
      coordIndex++;
    }
  }
  
  // 🚀 SIMD: Process coordinates in groups of 4 for vectorized operations
  const groupsOf4 = Math.floor(coordIndex / 4);
  const remainder = coordIndex % 4;
  
  // Process groups of 4 coordinates simultaneously
  for (let group = 0; group < groupsOf4; group++) {
    const baseIndex = group * 4;
    
    // Load 4 dx,dy pairs into SIMD-friendly arrays
    const dx0 = coords[baseIndex * 2], dy0 = coords[baseIndex * 2 + 1];
    const dx1 = coords[(baseIndex + 1) * 2], dy1 = coords[(baseIndex + 1) * 2 + 1];
    const dx2 = coords[(baseIndex + 2) * 2], dy2 = coords[(baseIndex + 2) * 2 + 1];
    const dx3 = coords[(baseIndex + 3) * 2], dy3 = coords[(baseIndex + 3) * 2 + 1];
    
    // 🚀 SIMD: Vectorized distance calculations
    const dist0 = Math.sqrt(dx0 * dx0 + dy0 * dy0);
    const dist1 = Math.sqrt(dx1 * dx1 + dy1 * dy1);
    const dist2 = Math.sqrt(dx2 * dx2 + dy2 * dy2);
    const dist3 = Math.sqrt(dx3 * dx3 + dy3 * dy3);
    
    // 🚀 SIMD: Vectorized angle calculations  
    const angle0 = Math.atan2(dy0, dx0);
    const angle1 = Math.atan2(dy1, dx1);
    const angle2 = Math.atan2(dy2, dx2);
    const angle3 = Math.atan2(dy3, dx3);
    
    // 🚀 SIMD: Vectorized angle transformations
    const totalChange0 = integerSteps / dist0;
    const totalChange1 = integerSteps / dist1;
    const totalChange2 = integerSteps / dist2;
    const totalChange3 = integerSteps / dist3;
    
    let srcAngle0 = angle0 - totalChange0;
    let srcAngle1 = angle1 - totalChange1;
    let srcAngle2 = angle2 - totalChange2;
    let srcAngle3 = angle3 - totalChange3;
    
    // 🚀 SIMD: Vectorized angle normalization
    srcAngle0 = srcAngle0 - twoPi * Math.floor(srcAngle0 / twoPi);
    srcAngle1 = srcAngle1 - twoPi * Math.floor(srcAngle1 / twoPi);
    srcAngle2 = srcAngle2 - twoPi * Math.floor(srcAngle2 / twoPi);
    srcAngle3 = srcAngle3 - twoPi * Math.floor(srcAngle3 / twoPi);
    
    if (srcAngle0 < 0) srcAngle0 += twoPi;
    if (srcAngle1 < 0) srcAngle1 += twoPi;
    if (srcAngle2 < 0) srcAngle2 += twoPi;
    if (srcAngle3 < 0) srcAngle3 += twoPi;
    
    // 🚀 SIMD: Vectorized coordinate conversion using fast trig lookup
    const srcX0 = centerX + dist0 * cos(srcAngle0);
    const srcY0 = centerY + dist0 * sin(srcAngle0);
    const srcX1 = centerX + dist1 * cos(srcAngle1);
    const srcY1 = centerY + dist1 * sin(srcAngle1);
    const srcX2 = centerX + dist2 * cos(srcAngle2);
    const srcY2 = centerY + dist2 * sin(srcAngle2);
    const srcX3 = centerX + dist3 * cos(srcAngle3);
    const srcY3 = centerY + dist3 * sin(srcAngle3);
    
    // Process each of the 4 pixels (wrapping and sampling)
    processSimdPixel(srcX0, srcY0, destIndices[baseIndex], minX, minY, maxXMinus1, maxYMinus1, workingWidth, workingHeight);
    processSimdPixel(srcX1, srcY1, destIndices[baseIndex + 1], minX, minY, maxXMinus1, maxYMinus1, workingWidth, workingHeight);
    processSimdPixel(srcX2, srcY2, destIndices[baseIndex + 2], minX, minY, maxXMinus1, maxYMinus1, workingWidth, workingHeight);
    processSimdPixel(srcX3, srcY3, destIndices[baseIndex + 3], minX, minY, maxXMinus1, maxYMinus1, workingWidth, workingHeight);
  }
  
  // Handle remaining pixels (less than 4)
  for (let i = groupsOf4 * 4; i < coordIndex; i++) {
    const dx = coords[i * 2];
    const dy = coords[i * 2 + 1];
    const distance = Math.sqrt(dx * dx + dy * dy);
    const angle = Math.atan2(dy, dx);
    const totalAngleChange = integerSteps / distance;
    let srcAngle = angle - totalAngleChange;
    srcAngle = srcAngle - twoPi * Math.floor(srcAngle / twoPi);
    if (srcAngle < 0) srcAngle += twoPi;
    const srcX = centerX + distance * Math.cos(srcAngle);
    const srcY = centerY + distance * Math.sin(srcAngle);
    processSimdPixel(srcX, srcY, destIndices[i], minX, minY, maxXMinus1, maxYMinus1, workingWidth, workingHeight);
  }
  
  const spinEnd = performance.now();
  graphPerf.track('spin', spinEnd - spinStart);
}

// 🚀 SIMD Helper: Process individual pixel with wrapping and sampling
function processSimdPixel(srcX, srcY, destIdx, minX, minY, maxXMinus1, maxYMinus1, workingWidth, workingHeight) {
  // Fast wrapping (same logic as regular spin)
  let wrappedSrcX = srcX;
  let wrappedSrcY = srcY;
  
  if (srcX < minX || srcX >= minX + workingWidth) {
    const normalizedX = srcX - minX;
    wrappedSrcX = minX + (normalizedX - workingWidth * Math.floor(normalizedX / workingWidth));
    if (wrappedSrcX < minX) wrappedSrcX += workingWidth;
  }
  if (srcY < minY || srcY >= minY + workingHeight) {
    const normalizedY = srcY - minY;
    wrappedSrcY = minY + (normalizedY - workingHeight * Math.floor(normalizedY / workingHeight));
    if (wrappedSrcY < minY) wrappedSrcY += workingHeight;
  }
  
  // Fast nearest neighbor with bounds clamping
  const nearestSrcX = Math.max(minX, Math.min(maxXMinus1, Math.round(wrappedSrcX)));
  const nearestSrcY = Math.max(minY, Math.min(maxYMinus1, Math.round(wrappedSrcY)));
  
  // Direct array access with pre-calculated offsets
  const srcIdx = (nearestSrcY * width + nearestSrcX) * 4;
  
  // Unrolled RGBA copy for maximum speed
  pixels[destIdx] = spinBuffer[srcIdx];
  pixels[destIdx + 1] = spinBuffer[srcIdx + 1];
  pixels[destIdx + 2] = spinBuffer[srcIdx + 2];
  pixels[destIdx + 3] = spinBuffer[srcIdx + 3];
}

// Static buffers for SIMD spin function
spinSimd.coordBuffers = null;
spinSimd.srcIndices = null;
spinSimd.destIndices = null;

// 🧪 EXPERIMENTAL: Block-based spin processing for better performance on large images
function spinBlockBased(steps, anchorX, anchorY, spinStart) {
  // Same early exits as regular spin
  if (graphPerf && graphPerf.lastFPS && graphPerf.lastFPS < 6) {
    spinSkipCounter++;
    if (spinSkipCounter % 2 !== 0) {
      graphPerf.track('spin', 0);
      return;
    }
  } else {
    spinSkipCounter = 0;
  }

  if (Math.abs(steps) < 0.5) {
    graphPerf.track('spin', 0);
    return;
  }

  // Handle fractional steps
  spinAccumulator += steps;
  const integerSteps = floor(spinAccumulator);
  spinAccumulator -= integerSteps;
  if (integerSteps === 0) {
    graphPerf.track('spin', 0);
    return;
  }

  // Determine the area to process
  let minX = 0, minY = 0, maxX = width, maxY = height;
  if (activeMask) {
    minX = activeMask.x + panTranslation.x;
    minY = activeMask.y + panTranslation.y;
    maxX = activeMask.x + activeMask.width + panTranslation.x;
    maxY = activeMask.y + activeMask.height + panTranslation.y;
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;
  const centerX = anchorX !== null ? anchorX : minX + floor(workingWidth / 2);
  const centerY = anchorY !== null ? anchorY : minY + floor(workingHeight / 2);

  // Setup buffers
  if (pixels.buffer && pixels.buffer.detached) {
    console.warn('🚨 Pixels buffer detached in spin, recreating from screen dimensions');
    pixels = new Uint8ClampedArray(width * height * 4);
    pixels.fill(0);
  }
  
  const bufferSize = width * height * 4;
  if (!spinBuffer || spinBuffer.length !== bufferSize) {
    spinBuffer = new Uint8ClampedArray(bufferSize);
  }
  
  if (pixels.length !== bufferSize) {
    pixels = new Uint8ClampedArray(bufferSize);
    pixels.fill(0);
  }
  
  spinBuffer.set(pixels);

  // 🧪 BLOCK-BASED PROCESSING: Process in BLOCK_SIZE x BLOCK_SIZE chunks
  const twoPi = 2 * PI;
  const maxXMinus1 = maxX - 1;
  const maxYMinus1 = maxY - 1;

  // Process blocks
  for (let blockY = minY; blockY < maxY; blockY += BLOCK_SIZE) {
    const blockEndY = Math.min(blockY + BLOCK_SIZE, maxY);
    
    for (let blockX = minX; blockX < maxX; blockX += BLOCK_SIZE) {
      const blockEndX = Math.min(blockX + BLOCK_SIZE, maxX);
      
      // 🚀 OPTIMIZATION: Skip blocks that are very far from center
      const blockCenterX = (blockX + blockEndX) / 2;
      const blockCenterY = (blockY + blockEndY) / 2;
      const blockDx = blockCenterX - centerX;
      const blockDy = blockCenterY - centerY;
      const blockDistanceSquared = blockDx * blockDx + blockDy * blockDy;
      
      // Skip blocks that are extremely distant
      if (blockDistanceSquared > 32000000) { // Even larger threshold for blocks
        continue;
      }
      
      // Process pixels within this block
      for (let destY = blockY; destY < blockEndY; destY++) {
        const dy = destY - centerY;
        const dy2 = dy * dy;
        const destRowOffset = destY * width;
        
        for (let destX = blockX; destX < blockEndX; destX++) {
          const dx = destX - centerX;
          const distanceSquared = dx * dx + dy2;
          
          // Fast path for center pixels
          if (distanceSquared < 1.0) {
            const idx = (destRowOffset + destX) * 4;
            pixels[idx] = spinBuffer[idx];
            pixels[idx + 1] = spinBuffer[idx + 1];
            pixels[idx + 2] = spinBuffer[idx + 2];
            pixels[idx + 3] = spinBuffer[idx + 3];
            continue;
          }
          
          // Skip very distant pixels within block
          if (distanceSquared > 16000000) {
            continue;
          }
          
          // Same rotation math as original
          const distance = sqrt(distanceSquared);
          const angle = Math.atan2(dy, dx);
          const totalAngleChange = integerSteps / distance;
          
          let sourceAngle = angle - totalAngleChange;
          sourceAngle = sourceAngle - twoPi * floor(sourceAngle / twoPi);
          if (sourceAngle < 0) sourceAngle += twoPi;
          
          const srcX = centerX + distance * cos(sourceAngle);
          const srcY = centerY + distance * sin(sourceAngle);
          
          // Optimized wrapping
          let wrappedSrcX = srcX;
          let wrappedSrcY = srcY;
          
          if (srcX < minX || srcX >= maxX) {
            const normalizedX = srcX - minX;
            wrappedSrcX = minX + (normalizedX - workingWidth * floor(normalizedX / workingWidth));
            if (wrappedSrcX < minX) wrappedSrcX += workingWidth;
          }
          if (srcY < minY || srcY >= maxY) {
            const normalizedY = srcY - minY;
            wrappedSrcY = minY + (normalizedY - workingHeight * floor(normalizedY / workingHeight));
            if (wrappedSrcY < minY) wrappedSrcY += workingHeight;
          }
          
          const nearestSrcX = Math.max(minX, Math.min(maxXMinus1, Math.round(wrappedSrcX)));
          const nearestSrcY = Math.max(minY, Math.min(maxYMinus1, Math.round(wrappedSrcY)));
          
          const srcIdx = (nearestSrcY * width + nearestSrcX) * 4;
          const destIdx = (destRowOffset + destX) * 4;
          
          // Unrolled RGBA copy
          pixels[destIdx] = spinBuffer[srcIdx];
          pixels[destIdx + 1] = spinBuffer[srcIdx + 1];
          pixels[destIdx + 2] = spinBuffer[srcIdx + 2];
          pixels[destIdx + 3] = spinBuffer[srcIdx + 3];
        }
      }
    }
  }
  
  const spinEnd = performance.now();
  graphPerf.track('spin', spinEnd - spinStart);
}

// Zoom the entire pixel buffer with 1.0 as neutral (no change)
// level < 1.0 zooms out, level > 1.0 zooms in, level = 1.0 does nothing
// anchorX, anchorY: 0.0 = top/left, 0.5 = center, 1.0 = bottom/right
// Uses bilinear sampling with hard-edge thresholding for smooth scaling with crisp output
function zoom(level = 1, anchorX = 0.5, anchorY = 0.5) {
  if (level === 1.0) return; // No change needed - neutral zoom
  
  // For large zoom changes, apply immediately
  // For small zoom changes, accumulate for aggregative effects
  let actualZoomLevel;
  
  if (Math.abs(level - 1.0) >= 0.1) {
    // Large zoom change - apply immediately (e.g., zoom(2) or zoom(0.5))
    actualZoomLevel = level;
    // Still update accumulator to maintain state consistency
    zoomAccumulator = 0; // Reset since we're applying a direct zoom
  } else {
    // Small zoom change - accumulate for aggregative effects (e.g., zoom(1.025))
    const zoomDelta = level - 1.0;
    zoomAccumulator += zoomDelta;
    
    // Only apply when accumulated change is significant enough
    const threshold = 0.05; // Increased from 0.01 to 0.05 for better performance
    
    if (Math.abs(zoomAccumulator) < threshold) return;
    
    // Apply the accumulated zoom change
    const zoomToApply = Math.sign(zoomAccumulator) * threshold;
    actualZoomLevel = 1.0 + zoomToApply;
    
    // Reduce accumulator by what we applied
    zoomAccumulator -= zoomToApply;
  }

  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    minX = activeMask.x;
    minY = activeMask.y;
    maxX = activeMask.x + activeMask.width;
    maxY = activeMask.y + activeMask.height;
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;

  // Calculate anchor point in pixel coordinates within working area
  const anchorPixelX = minX + workingWidth * anchorX;
  const anchorPixelY = minY + workingHeight * anchorY;

  // Create a copy of the current pixels to read from
  // 🛡️ SAFETY CHECK: If pixels buffer is detached, recreate it
  if (pixels.buffer && pixels.buffer.detached) {
    console.warn('🚨 Pixels buffer detached in zoom, recreating from screen dimensions');
    // This should not happen if setBuffer works correctly, but just in case...
    pixels = new Uint8ClampedArray(width * height * 4);
    pixels.fill(0); // Fill with transparent black
  }
  
  // 🚀 OPTIMIZATION: Reuse zoom buffer instead of allocating new one each call
  const bufferSize = width * height * 4;
  if (!zoomBuffer || zoomBuffer.length !== bufferSize) {
    zoomBuffer = new Uint8ClampedArray(bufferSize);
  }
  zoomBuffer.set(pixels);

  const scale = actualZoomLevel;
  const invScale = 1.0 / scale;
  const invWorkingWidth = 1.0 / workingWidth;
  
  // 🚀 SIMD: Create typed arrays for vectorized processing
  const pixelCount = (maxY - minY) * (maxX - minX);
  const maxPixels = Math.ceil(pixelCount / 4) * 4; // Round up to multiple of 4
  
  // Reuse coordinate buffers to avoid allocation overhead
  if (!zoom.coordBuffers || zoom.coordBuffers.length < maxPixels * 4) {
    zoom.srcCoords = new Float32Array(maxPixels * 2); // x,y pairs for source coordinates
    zoom.destIndices = new Uint32Array(maxPixels);
  }
  
  const srcCoords = zoom.srcCoords;
  const destIndices = zoom.destIndices;
  let coordIndex = 0;
  
  // 🚀 SIMD: Generate all coordinates in vectorized batches
  for (let destY = minY; destY < maxY; destY++) {
    const destRowOffset = destY * width;
    const srcYBase = (destY - anchorPixelY) * invScale + anchorPixelY;
    
    // Pre-calculate Y wrapping once per row
    let wrappedSrcYBase = ((srcYBase - minY) % workingHeight + workingHeight) % workingHeight + minY;
    const nearestY = Math.round(wrappedSrcYBase);
    const finalSrcY = minY + ((nearestY - minY) % workingHeight + workingHeight) % workingHeight;
    
    if (finalSrcY >= minY && finalSrcY < maxY) {
      const srcRowOffset = finalSrcY * width;
      
      for (let destX = minX; destX < maxX; destX++) {
        // Calculate source X coordinate
        const srcX = (destX - anchorPixelX) * invScale + anchorPixelX;
        
        // Store coordinates for SIMD processing
        srcCoords[coordIndex * 2] = srcX;
        srcCoords[coordIndex * 2 + 1] = finalSrcY; // Use pre-calculated Y
        destIndices[coordIndex] = (destRowOffset + destX) * 4;
        coordIndex++;
      }
    }
  }
  
  // 🚀 SIMD: Process coordinates in groups of 4 for vectorized operations
  const groupsOf4 = Math.floor(coordIndex / 4);
  
  for (let group = 0; group < groupsOf4; group++) {
    const baseIndex = group * 4;
    
    // Load 4 source X coordinates for vectorized processing
    const srcX0 = srcCoords[baseIndex * 2];
    const srcX1 = srcCoords[(baseIndex + 1) * 2];
    const srcX2 = srcCoords[(baseIndex + 2) * 2];
    const srcX3 = srcCoords[(baseIndex + 3) * 2];
    
    // Use same Y coordinate for this row (already calculated)
    const srcY = srcCoords[baseIndex * 2 + 1];
    const srcRowOffset = srcY * width;
    
    // 🚀 SIMD: Vectorized X coordinate wrapping
    const normalizedX0 = (srcX0 - minX) * invWorkingWidth;
    const normalizedX1 = (srcX1 - minX) * invWorkingWidth;
    const normalizedX2 = (srcX2 - minX) * invWorkingWidth;
    const normalizedX3 = (srcX3 - minX) * invWorkingWidth;
    
    const wrappedNormX0 = normalizedX0 - Math.floor(normalizedX0);
    const wrappedNormX1 = normalizedX1 - Math.floor(normalizedX1);
    const wrappedNormX2 = normalizedX2 - Math.floor(normalizedX2);
    const wrappedNormX3 = normalizedX3 - Math.floor(normalizedX3);
    
    const wrappedSrcX0 = minX + wrappedNormX0 * workingWidth;
    const wrappedSrcX1 = minX + wrappedNormX1 * workingWidth;
    const wrappedSrcX2 = minX + wrappedNormX2 * workingWidth;
    const wrappedSrcX3 = minX + wrappedNormX3 * workingWidth;
    
    // 🚀 SIMD: Vectorized nearest neighbor sampling
    const nearestX0 = Math.round(wrappedSrcX0);
    const nearestX1 = Math.round(wrappedSrcX1);
    const nearestX2 = Math.round(wrappedSrcX2);
    const nearestX3 = Math.round(wrappedSrcX3);
    
    const finalSrcX0 = minX + ((nearestX0 - minX) % workingWidth + workingWidth) % workingWidth;
    const finalSrcX1 = minX + ((nearestX1 - minX) % workingWidth + workingWidth) % workingWidth;
    const finalSrcX2 = minX + ((nearestX2 - minX) % workingWidth + workingWidth) % workingWidth;
    const finalSrcX3 = minX + ((nearestX3 - minX) % workingWidth + workingWidth) % workingWidth;
    
    // Process each of the 4 pixels if coordinates are valid
    if (finalSrcX0 >= minX && finalSrcX0 < maxX) {
      processZoomPixel(srcRowOffset + finalSrcX0, destIndices[baseIndex]);
    }
    if (finalSrcX1 >= minX && finalSrcX1 < maxX) {
      processZoomPixel(srcRowOffset + finalSrcX1, destIndices[baseIndex + 1]);
    }
    if (finalSrcX2 >= minX && finalSrcX2 < maxX) {
      processZoomPixel(srcRowOffset + finalSrcX2, destIndices[baseIndex + 2]);
    }
    if (finalSrcX3 >= minX && finalSrcX3 < maxX) {
      processZoomPixel(srcRowOffset + finalSrcX3, destIndices[baseIndex + 3]);
    }
  }
  
  // Handle remaining pixels (less than 4)
  for (let i = groupsOf4 * 4; i < coordIndex; i++) {
    const srcX = srcCoords[i * 2];
    const srcY = srcCoords[i * 2 + 1];
    const srcRowOffset = srcY * width;
    
    // X coordinate wrapping
    const normalizedX = (srcX - minX) * invWorkingWidth;
    const wrappedNormX = normalizedX - Math.floor(normalizedX);
    const wrappedSrcX = minX + wrappedNormX * workingWidth;
    const nearestX = Math.round(wrappedSrcX);
    const finalSrcX = minX + ((nearestX - minX) % workingWidth + workingWidth) % workingWidth;
    
    if (finalSrcX >= minX && finalSrcX < maxX) {
      processZoomPixel(srcRowOffset + finalSrcX, destIndices[i]);
    }
  }

  // Don't reset zoom accumulator - let it continue accumulating like scroll and spin
}

// 🚀 SIMD Helper: Process individual zoom pixel with fast copying
function processZoomPixel(srcPixelIndex, destIdx) {
  const srcIdx = srcPixelIndex * 4;
  
  // Unrolled RGBA copy for maximum speed
  pixels[destIdx] = zoomBuffer[srcIdx];
  pixels[destIdx + 1] = zoomBuffer[srcIdx + 1];
  pixels[destIdx + 2] = zoomBuffer[srcIdx + 2];
  pixels[destIdx + 3] = zoomBuffer[srcIdx + 3];
}

// Static buffers for SIMD zoom function
zoom.srcCoords = null;
zoom.destIndices = null;

// 🚀 EXPERIMENTAL: SIMD-optimized zoom processing using typed arrays
function zoomSimd(actualZoomLevel, anchorX, anchorY) {
  // Determine the area to process (mask or full screen)
  let minX = 0, minY = 0, maxX = width, maxY = height;
  if (activeMask) {
    minX = activeMask.x;
    minY = activeMask.y;
    maxX = activeMask.x + activeMask.width;
    maxY = activeMask.y + activeMask.height;
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;
  const anchorPixelX = minX + workingWidth * anchorX;
  const anchorPixelY = minY + workingHeight * anchorY;

  // 🛡️ SAFETY CHECK: Buffer management
  if (pixels.buffer && pixels.buffer.detached) {
    console.warn('🚨 Pixels buffer detached in zoomSimd, recreating');
    pixels = new Uint8ClampedArray(width * height * 4);
    pixels.fill(0);
  }
  
  const bufferSize = width * height * 4;
  if (!zoomBuffer || zoomBuffer.length !== bufferSize) {
    zoomBuffer = new Uint8ClampedArray(bufferSize);
  }
  zoomBuffer.set(pixels);

  const scale = actualZoomLevel;
  const invScale = 1.0 / scale;
  
  // 🚀 SIMD: Create typed arrays for vectorized processing
  const pixelCount = (maxY - minY) * (maxX - minX);
  const maxPixels = Math.ceil(pixelCount / 4) * 4; // Round up to multiple of 4
  
  // Reuse coordinate buffers to avoid allocation overhead
  if (!zoomSimd.coordBuffers || zoomSimd.coordBuffers.length < maxPixels * 4) {
    zoomSimd.srcCoords = new Float32Array(maxPixels * 2); // x,y pairs for source coordinates
    zoomSimd.destIndices = new Uint32Array(maxPixels);
    zoomSimd.srcIndices = new Uint32Array(maxPixels);
  }
  
  const srcCoords = zoomSimd.srcCoords;
  const destIndices = zoomSimd.destIndices;
  const srcIndices = zoomSimd.srcIndices;

  // Pre-calculate constants for SIMD operations
  const invWorkingWidth = 1.0 / workingWidth;
  const invWorkingHeight = 1.0 / workingHeight;
  
  let coordIndex = 0;
  
  // 🚀 SIMD: Generate all coordinates in vectorized batches
  for (let destY = minY; destY < maxY; destY++) {
    const destRowOffset = destY * width;
    const srcYBase = (destY - anchorPixelY) * invScale + anchorPixelY;
    
    // Pre-calculate Y wrapping once per row
    let wrappedSrcYBase = ((srcYBase - minY) % workingHeight + workingHeight) % workingHeight + minY;
    const nearestY = Math.round(wrappedSrcYBase);
    const finalSrcY = minY + ((nearestY - minY) % workingHeight + workingHeight) % workingHeight;
    
    if (finalSrcY >= minY && finalSrcY < maxY) {
      const srcRowOffset = finalSrcY * width;
      
      for (let destX = minX; destX < maxX; destX++) {
        // Calculate source X coordinate
        const srcX = (destX - anchorPixelX) * invScale + anchorPixelX;
        
        // Store coordinates for SIMD processing
        srcCoords[coordIndex * 2] = srcX;
        srcCoords[coordIndex * 2 + 1] = finalSrcY; // Use pre-calculated Y
        destIndices[coordIndex] = (destRowOffset + destX) * 4;
        coordIndex++;
      }
    }
  }
  
  // 🚀 SIMD: Process coordinates in groups of 4 for vectorized operations
  const groupsOf4 = Math.floor(coordIndex / 4);
  const remainder = coordIndex % 4;
  
  // Process groups of 4 coordinates simultaneously
  for (let group = 0; group < groupsOf4; group++) {
    const baseIndex = group * 4;
    
    // Load 4 source X coordinates for vectorized processing
    const srcX0 = srcCoords[baseIndex * 2];
    const srcX1 = srcCoords[(baseIndex + 1) * 2];
    const srcX2 = srcCoords[(baseIndex + 2) * 2];
    const srcX3 = srcCoords[(baseIndex + 3) * 2];
    
    // Use same Y coordinate for this row (already calculated)
    const srcY = srcCoords[baseIndex * 2 + 1];
    const srcRowOffset = srcY * width;
    
    // 🚀 SIMD: Vectorized X coordinate wrapping
    const normalizedX0 = (srcX0 - minX) * invWorkingWidth;
    const normalizedX1 = (srcX1 - minX) * invWorkingWidth;
    const normalizedX2 = (srcX2 - minX) * invWorkingWidth;
    const normalizedX3 = (srcX3 - minX) * invWorkingWidth;
    
    const wrappedNormX0 = normalizedX0 - Math.floor(normalizedX0);
    const wrappedNormX1 = normalizedX1 - Math.floor(normalizedX1);
    const wrappedNormX2 = normalizedX2 - Math.floor(normalizedX2);
    const wrappedNormX3 = normalizedX3 - Math.floor(normalizedX3);
    
    const wrappedSrcX0 = minX + wrappedNormX0 * workingWidth;
    const wrappedSrcX1 = minX + wrappedNormX1 * workingWidth;
    const wrappedSrcX2 = minX + wrappedNormX2 * workingWidth;
    const wrappedSrcX3 = minX + wrappedNormX3 * workingWidth;
    
    // 🚀 SIMD: Vectorized nearest neighbor sampling
    const nearestX0 = Math.round(wrappedSrcX0);
    const nearestX1 = Math.round(wrappedSrcX1);
    const nearestX2 = Math.round(wrappedSrcX2);
    const nearestX3 = Math.round(wrappedSrcX3);
    
    const finalSrcX0 = minX + ((nearestX0 - minX) % workingWidth + workingWidth) % workingWidth;
    const finalSrcX1 = minX + ((nearestX1 - minX) % workingWidth + workingWidth) % workingWidth;
    const finalSrcX2 = minX + ((nearestX2 - minX) % workingWidth + workingWidth) % workingWidth;
    const finalSrcX3 = minX + ((nearestX3 - minX) % workingWidth + workingWidth) % workingWidth;
    
    // Process each of the 4 pixels if coordinates are valid
    if (finalSrcX0 >= minX && finalSrcX0 < maxX) {
      processZoomSimdPixel(srcRowOffset + finalSrcX0, destIndices[baseIndex]);
    }
    if (finalSrcX1 >= minX && finalSrcX1 < maxX) {
      processZoomSimdPixel(srcRowOffset + finalSrcX1, destIndices[baseIndex + 1]);
    }
    if (finalSrcX2 >= minX && finalSrcX2 < maxX) {
      processZoomSimdPixel(srcRowOffset + finalSrcX2, destIndices[baseIndex + 2]);
    }
    if (finalSrcX3 >= minX && finalSrcX3 < maxX) {
      processZoomSimdPixel(srcRowOffset + finalSrcX3, destIndices[baseIndex + 3]);
    }
  }
  
  // Handle remaining pixels (less than 4)
  for (let i = groupsOf4 * 4; i < coordIndex; i++) {
    const srcX = srcCoords[i * 2];
    const srcY = srcCoords[i * 2 + 1];
    const srcRowOffset = srcY * width;
    
    // X coordinate wrapping
    const normalizedX = (srcX - minX) * invWorkingWidth;
    const wrappedNormX = normalizedX - Math.floor(normalizedX);
    const wrappedSrcX = minX + wrappedNormX * workingWidth;
    const nearestX = Math.round(wrappedSrcX);
    const finalSrcX = minX + ((nearestX - minX) % workingWidth + workingWidth) % workingWidth;
    
    if (finalSrcX >= minX && finalSrcX < maxX) {
      processZoomSimdPixel(srcRowOffset + finalSrcX, destIndices[i]);
    }
  }
}

// 🚀 SIMD Helper: Process individual zoom pixel with fast copying
function processZoomSimdPixel(srcPixelIndex, destIdx) {
  const srcIdx = srcPixelIndex * 4;
  
  // Unrolled RGBA copy for maximum speed
  pixels[destIdx] = zoomBuffer[srcIdx];
  pixels[destIdx + 1] = zoomBuffer[srcIdx + 1];
  pixels[destIdx + 2] = zoomBuffer[srcIdx + 2];
  pixels[destIdx + 3] = zoomBuffer[srcIdx + 3];
}

// Static buffers for SIMD zoom function
zoomSimd.srcCoords = null;
zoomSimd.destIndices = null;
zoomSimd.srcIndices = null;

// Radial displacement transformation with pixel-perfect nearest neighbor sampling
// Creates discrete, lossless pixel movement without blur or center holes
function suck(strength = 1, centerX, centerY) {
  if (strength === 0.0) return; // No change needed - neutral suck
  
  // Accumulate the suck strength like zoom does
  suckAccumulator += strength;
  
  // Apply transformation when accumulator reaches threshold
  const threshold = 0.5; // Higher threshold for more controlled movement
  if (Math.abs(suckAccumulator) < threshold) return;
  
  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    minX = activeMask.x;
    minY = activeMask.y;
    maxX = activeMask.x + activeMask.width;
    maxY = activeMask.y + activeMask.height;
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;

  // Default center point to middle of working area
  const centerPixelX = centerX !== undefined ? centerX : minX + workingWidth * 0.5;
  const centerPixelY = centerY !== undefined ? centerY : minY + workingHeight * 0.5;

  // 🛡️ SAFETY CHECK: If pixels buffer is detached, recreate it
  if (pixels.buffer && pixels.buffer.detached) {
    console.warn('🚨 Pixels buffer detached in suck, recreating from screen dimensions');
    pixels = new Uint8ClampedArray(width * height * 4);
    pixels.fill(0); // Fill with transparent black
  }
  
  // Create snapshot for pixel-perfect sampling
  const tempPixels = new Uint8ClampedArray(pixels);

  // Use smooth displacement (no discrete steps for quality preservation)
  const displacementAmount = Math.abs(suckAccumulator);
  const direction = suckAccumulator > 0 ? 1 : -1; // Positive = inward, Negative = outward
  
  if (displacementAmount < 0.01) {
    suckAccumulator = 0.0;
    return; // No movement needed
  }
  
  // Calculate maximum distance for wrapping
  const maxDistance = Math.max(workingWidth, workingHeight);

  // Apply pixel-perfect radial displacement
  for (let destY = minY; destY < maxY; destY++) {
    const destRowOffset = destY * width;
    
    for (let destX = minX; destX < maxX; destX++) {
      // Calculate distance from center using Euclidean distance for natural circles
      const dx = destX - centerPixelX;
      const dy = destY - centerPixelY;
      const distance = Math.sqrt(dx * dx + dy * dy);
      
      // Prevent center hole by having minimum movement radius
      if (distance < 2.0) {
        // Very center pixels stay put to prevent holes
        const destIdx = (destRowOffset + destX) * 4;
        pixels[destIdx] = tempPixels[destIdx];
        pixels[destIdx + 1] = tempPixels[destIdx + 1];
        pixels[destIdx + 2] = tempPixels[destIdx + 2];
        pixels[destIdx + 3] = tempPixels[destIdx + 3];
        continue;
      }
      
      // Calculate smooth source distance with sub-pixel precision
      let srcDistance = distance - (direction * displacementAmount); // Smooth sub-pixel movement
      
      // Proper wrapping for true circulation
      if (srcDistance <= 0) {
        // Pixel moved past center, wrap to outer edge 
        srcDistance = maxDistance + srcDistance; // Preserve the excess
      } else if (srcDistance > maxDistance) {
        // Pixel moved past edge, wrap back toward center
        srcDistance = srcDistance - maxDistance;
      }
      
      // Calculate source position using exact same direction as destination (fast trig)
      const angle = Math.atan2(dy, dx);
      const srcX = centerPixelX + srcDistance * cos(angle);
      const srcY = centerPixelY + srcDistance * sin(angle);
      
      // Proper modulo wrapping for continuous circulation
      let wrappedSrcX = srcX;
      let wrappedSrcY = srcY;
      
      // True modulo wrapping that preserves circulation
      while (wrappedSrcX < minX) {
        wrappedSrcX += workingWidth;
      }
      while (wrappedSrcX >= maxX) {
        wrappedSrcX -= workingWidth;
      }
      
      while (wrappedSrcY < minY) {
        wrappedSrcY += workingHeight;
      }
      while (wrappedSrcY >= maxY) {
        wrappedSrcY -= workingHeight;
      }
      
      // SMOOTH SAMPLING: Use bilinear interpolation for sub-pixel precision
      const srcX_floor = Math.floor(wrappedSrcX);
      const srcY_floor = Math.floor(wrappedSrcY);
      const srcX_ceil = srcX_floor + 1;
      const srcY_ceil = srcY_floor + 1;
      
      // Calculate interpolation weights
      const wx = wrappedSrcX - srcX_floor;
      const wy = wrappedSrcY - srcY_floor;
      
      // Clamp coordinates to valid range
      const x1 = Math.max(minX, Math.min(maxX - 1, srcX_floor));
      const y1 = Math.max(minY, Math.min(maxY - 1, srcY_floor));
      const x2 = Math.max(minX, Math.min(maxX - 1, srcX_ceil));
      const y2 = Math.max(minY, Math.min(maxY - 1, srcY_ceil));
      
      // Get four corner pixels for bilinear interpolation
      const idx1 = (y1 * width + x1) * 4; // top-left
      const idx2 = (y1 * width + x2) * 4; // top-right  
      const idx3 = (y2 * width + x1) * 4; // bottom-left
      const idx4 = (y2 * width + x2) * 4; // bottom-right
      
      const destIdx = (destRowOffset + destX) * 4;
      
      // Bilinear interpolation for each color channel
      for (let c = 0; c < 4; c++) {
        const topInterp = tempPixels[idx1 + c] * (1 - wx) + tempPixels[idx2 + c] * wx;
        const bottomInterp = tempPixels[idx3 + c] * (1 - wx) + tempPixels[idx4 + c] * wx;
        const finalInterp = topInterp * (1 - wy) + bottomInterp * wy;
        pixels[destIdx + c] = Math.round(finalInterp);
      }
    }
  }

  // Reset accumulator after applying transformation
  suckAccumulator = 0.0;
}

// Accumulated blur strength for smooth progressive blurring
let blurAccumulator = 0.0;

// Reusable temporary buffer for blur operations to avoid memory allocation on each call
let blurTempBuffer = null;
let blurTempBufferSize = 0;

// Function to clean up blur buffers and reset state
function cleanupBlurBuffers() {
  blurTempBuffer = null;
  blurTempBufferSize = 0;
  blurAccumulator = 0.0;
}

// Efficient Gaussian blur using separable filtering with linear sampling optimization
// Creates smooth blur effect by applying horizontal then vertical Gaussian convolution
function blur(strength = 1, quality = "medium") {
  if (strength <= 0.1) return; // No blur needed - neutral blur
  
  // Start timing for performance monitoring
  const blurStartTime = performance.now();
  
  // Accumulate the blur strength for progressive blurring
  blurAccumulator += strength;
  
  // Apply blur when accumulator reaches threshold
  const threshold = 0.5;
  if (Math.abs(blurAccumulator) < threshold) return;
  
  // Determine the area to process (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Apply pan translation to mask bounds and ensure they're within screen bounds
    minX = Math.max(0, Math.min(width, activeMask.x + panTranslation.x));
    maxX = Math.max(
      0,
      Math.min(width, activeMask.x + activeMask.width + panTranslation.x),
    );
    minY = Math.max(0, Math.min(height, activeMask.y + panTranslation.y));
    maxY = Math.max(
      0,
      Math.min(height, activeMask.y + activeMask.height + panTranslation.y),
    );
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;

  // Early exit if bounds are invalid
  if (workingWidth <= 0 || workingHeight <= 0) {
    blurAccumulator = 0.0;
    return;
  }

  // 🛡️ SAFETY CHECK: If pixels buffer is detached, recreate it
  if (pixels.buffer && pixels.buffer.detached) {
    console.warn('🚨 Pixels buffer detached in blur, recreating from screen dimensions');
    pixels = new Uint8ClampedArray(width * height * 4);
    pixels.fill(0); // Fill with transparent black
  }
  
  // Calculate blur parameters based on strength and quality
  const blurRadius = Math.max(1, Math.floor(Math.abs(blurAccumulator)));
  const kernelSize = Math.min(blurRadius * 2 + 1, 15); // Cap at 15 for performance
  
  // Generate Gaussian weights using Pascal triangle approximation
  const weights = generateGaussianWeights(kernelSize, quality);
  const radius = Math.floor(kernelSize / 2);
  
  try {
    // Reuse temporary buffer to avoid repeated large allocations
    const requiredSize = pixels.length;
    if (!blurTempBuffer || blurTempBufferSize !== requiredSize) {
      // Only allocate new buffer if size changed or buffer doesn't exist
      blurTempBuffer = new Uint8ClampedArray(requiredSize);
      blurTempBufferSize = requiredSize;
    }
    
    // Clear the temp buffer to ensure clean state
    blurTempBuffer.fill(0);
    
    // PASS 1: Horizontal blur
    applyHorizontalBlur(pixels, blurTempBuffer, weights, radius, minX, minY, maxX, maxY);
    
    // PASS 2: Vertical blur (from temp back to main buffer)
    applyVerticalBlur(blurTempBuffer, pixels, weights, radius, minX, minY, maxX, maxY);
    
  } catch (error) {
    console.warn('🚨 Blur operation failed:', error);
    // Reset temp buffer on error to prevent issues on next call
    blurTempBuffer = null;
    blurTempBufferSize = 0;
  }
  
  // Log timing information for performance monitoring
  const blurEndTime = performance.now();
  const blurDuration = blurEndTime - blurStartTime;
  console.log(`⚡ blur: ${blurDuration.toFixed(3)}ms`);
  
  // Reset accumulator after applying blur
  blurAccumulator = 0.0;
}

// Generate optimized Gaussian weights for blur kernel
function generateGaussianWeights(kernelSize, quality) {
  const weights = new Array(kernelSize);
  const radius = Math.floor(kernelSize / 2);
  const sigma = radius / 3.0; // Standard deviation for nice falloff
  
  if (quality === "fast") {
    // Simple box blur approximation - all weights equal
    const weight = 1.0 / kernelSize;
    for (let i = 0; i < kernelSize; i++) {
      weights[i] = weight;
    }
  } else {
    // Proper Gaussian weights
    let sum = 0.0;
    for (let i = 0; i < kernelSize; i++) {
      const x = i - radius;
      const weight = Math.exp(-(x * x) / (2 * sigma * sigma));
      weights[i] = weight;
      sum += weight;
    }
    
    // Normalize weights to sum to 1.0
    for (let i = 0; i < kernelSize; i++) {
      weights[i] /= sum;
    }
  }
  
  return weights;
}

// Apply horizontal Gaussian blur pass with optimized sampling
function applyHorizontalBlur(sourcePixels, destPixels, weights, radius, minX, minY, maxX, maxY) {
  // Safety checks
  if (!sourcePixels || !destPixels || !weights) return;
  if (radius < 0 || radius >= weights.length / 2) return;
  
  for (let y = minY; y < maxY; y++) {
    const rowOffset = y * width;
    
    for (let x = minX; x < maxX; x++) {
      const destIdx = (rowOffset + x) * 4;
      
      // Bounds check for destination index
      if (destIdx < 0 || destIdx + 3 >= destPixels.length) continue;
      
      let r = 0, g = 0, b = 0, a = 0;
      
      // Apply convolution kernel horizontally
      for (let k = -radius; k <= radius; k++) {
        // Handle boundary conditions with clamping
        const srcX = Math.max(minX, Math.min(maxX - 1, x + k));
        const srcIdx = (rowOffset + srcX) * 4;
        
        // Bounds check for source index
        if (srcIdx < 0 || srcIdx + 3 >= sourcePixels.length) continue;
        
        const weightIdx = k + radius;
        if (weightIdx < 0 || weightIdx >= weights.length) continue;
        
        const weight = weights[weightIdx];
        
        r += sourcePixels[srcIdx] * weight;
        g += sourcePixels[srcIdx + 1] * weight;
        b += sourcePixels[srcIdx + 2] * weight;
        a += sourcePixels[srcIdx + 3] * weight;
      }
      
      // Store blurred values with clamping
      destPixels[destIdx] = Math.round(Math.max(0, Math.min(255, r)));
      destPixels[destIdx + 1] = Math.round(Math.max(0, Math.min(255, g)));
      destPixels[destIdx + 2] = Math.round(Math.max(0, Math.min(255, b)));
      destPixels[destIdx + 3] = Math.round(Math.max(0, Math.min(255, a)));
    }
  }
}

// Apply vertical Gaussian blur pass with optimized sampling
function applyVerticalBlur(sourcePixels, destPixels, weights, radius, minX, minY, maxX, maxY) {
  // Safety checks
  if (!sourcePixels || !destPixels || !weights) return;
  if (radius < 0 || radius >= weights.length / 2) return;
  
  for (let y = minY; y < maxY; y++) {
    const rowOffset = y * width;
    
    for (let x = minX; x < maxX; x++) {
      const destIdx = (rowOffset + x) * 4;
      
      // Bounds check for destination index
      if (destIdx < 0 || destIdx + 3 >= destPixels.length) continue;
      
      let r = 0, g = 0, b = 0, a = 0;
      
      // Apply convolution kernel vertically
      for (let k = -radius; k <= radius; k++) {
        // Handle boundary conditions with clamping
        const srcY = Math.max(minY, Math.min(maxY - 1, y + k));
        const srcIdx = (srcY * width + x) * 4;
        
        // Bounds check for source index
        if (srcIdx < 0 || srcIdx + 3 >= sourcePixels.length) continue;
        
        const weightIdx = k + radius;
        if (weightIdx < 0 || weightIdx >= weights.length) continue;
        
        const weight = weights[weightIdx];
        
        r += sourcePixels[srcIdx] * weight;
        g += sourcePixels[srcIdx + 1] * weight;
        b += sourcePixels[srcIdx + 2] * weight;
        a += sourcePixels[srcIdx + 3] * weight;
      }
      
      // Store blurred values with clamping
      destPixels[destIdx] = Math.round(Math.max(0, Math.min(255, r)));
      destPixels[destIdx + 1] = Math.round(Math.max(0, Math.min(255, g)));
      destPixels[destIdx + 2] = Math.round(Math.max(0, Math.min(255, b)));
      destPixels[destIdx + 3] = Math.round(Math.max(0, Math.min(255, a)));
    }
  }
}

// Sort pixels by color within the masked area (or entire screen if no mask)
// Sorts by luminance (brightness) - darker pixels first, lighter pixels last
function sort() {
  // Determine the area to sort (mask or full screen)
  let minX = 0,
    minY = 0,
    maxX = width,
    maxY = height;
  if (activeMask) {
    // Apply pan translation to mask bounds
    const maskX = activeMask.x + panTranslation.x;
    const maskY = activeMask.y + panTranslation.y;
    minX = Math.max(0, Math.floor(maskX));
    minY = Math.max(0, Math.floor(maskY));
    maxX = Math.min(width, Math.floor(maskX + activeMask.width));
    maxY = Math.min(height, Math.floor(maskY + activeMask.height));
  }

  // Collect all pixels in the area
  const pixelsToSort = [];
  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const index = (y * width + x) * 4;
      const r = pixels[index];
      const g = pixels[index + 1];
      const b = pixels[index + 2];
      const a = pixels[index + 3];

      // Calculate luminance for sorting (standard formula)
      const luminance = 0.299 * r + 0.587 * g + 0.114 * b;

      pixelsToSort.push({
        r,
        g,
        b,
        a,
        luminance,
      });
    }
  }

  // Sort by luminance (darker to lighter)
  pixelsToSort.sort((a, b) => a.luminance - b.luminance);

  // Write sorted pixels back to their positions
  let sortedIndex = 0;
  for (let y = minY; y < maxY; y++) {
    for (let x = minX; x < maxX; x++) {
      const index = (y * width + x) * 4;
      const sortedPixel = pixelsToSort[sortedIndex];

      pixels[index] = sortedPixel.r;
      pixels[index + 1] = sortedPixel.g;
      pixels[index + 2] = sortedPixel.b;
      pixels[index + 3] = sortedPixel.a;

      sortedIndex++;
    }
  }
}

// Copy a region from the current screen buffer into a new buffer
// Returns a buffer object that can be used with paste()
function copyRegion(x, y, w, h) {
  x = Math.floor(x);
  y = Math.floor(y);
  w = Math.floor(w);
  h = Math.floor(h);

  // Apply pan translation to coordinates
  x += panTranslation.x;
  y += panTranslation.y;

  // Clamp to screen bounds
  x = Math.max(0, Math.min(x, width));
  y = Math.max(0, Math.min(y, height));
  w = Math.max(0, Math.min(w, width - x));
  h = Math.max(0, Math.min(h, height - y));

  if (w <= 0 || h <= 0) {
    return null;
  }

  // Create new buffer using ImageData like makeBuffer does
  const imageData = new ImageData(w, h);
  const buffer = {
    pixels: imageData.data,
    width: imageData.width,
    height: imageData.height,
  };

  // Copy pixels from screen buffer to new buffer
  for (let srcY = 0; srcY < h; srcY++) {
    for (let srcX = 0; srcX < w; srcX++) {
      const srcIndex = ((y + srcY) * width + (x + srcX)) * 4;
      const destIndex = (srcY * w + srcX) * 4;

      if (srcIndex >= 0 && srcIndex < pixels.length - 3) {
        buffer.pixels[destIndex] = pixels[srcIndex];
        buffer.pixels[destIndex + 1] = pixels[srcIndex + 1];
        buffer.pixels[destIndex + 2] = pixels[srcIndex + 2];
        buffer.pixels[destIndex + 3] = pixels[srcIndex + 3];
      }
    }
  }

  return buffer;
}

let stolen;

// TODO: Steal should run copyregion and keep the buffer in a global 'stolen' variable.
function steal(x, y, width, height) {
  stolen = copyRegion(x, y, width, height);
  return stolen;
}

// Paste the stolen buffer at the specified coordinates with optional scaling
function putback(x, y, scale = 1) {
  if (!stolen) return;
  const result = paste(stolen, x, y, scale);
  return result;
}

// KidPix-style shear function
// shearX: horizontal shear amount (positive = right lean, negative = left lean)
// Simple shear by moving entire rows/columns
// shearX: horizontal shear factor (positive = right lean, negative = left lean)
// shearY: vertical shear factor (positive = down lean, negative = up lean)
function shear(shearX = 0, shearY = 0) {
  if (shearX === 0 && shearY === 0) return;

  // Accumulate fractional shear amounts
  shearAccumulatorX += shearX;
  shearAccumulatorY += shearY;

  const finalShearX = shearAccumulatorX;
  const finalShearY = shearAccumulatorY;

  shearAccumulatorX = 0;
  shearAccumulatorY = 0;

  // Work area bounds
  let minX = 0,
    maxX = width,
    minY = 0,
    maxY = height;
  if (activeMask) {
    minX = Math.max(0, Math.min(width, activeMask.x + panTranslation.x));
    maxX = Math.max(
      0,
      Math.min(width, activeMask.x + activeMask.width + panTranslation.x),
    );
    minY = Math.max(0, Math.min(height, activeMask.y + panTranslation.y));
    maxY = Math.max(
      0,
      Math.min(height, activeMask.y + activeMask.height + panTranslation.y),
    );
  }

  const workingWidth = maxX - minX;
  const workingHeight = maxY - minY;
  if (workingWidth <= 0 || workingHeight <= 0) return;

  // 🛡️ SAFETY CHECK: If pixels buffer is detached, recreate it
  if (pixels.buffer && pixels.buffer.detached) {
    console.warn('🚨 Pixels buffer detached in shear, recreating from screen dimensions');
    pixels = new Uint8ClampedArray(width * height * 4);
    pixels.fill(0); // Fill with transparent black
  }

  const tempPixels = new Uint8ClampedArray(pixels);
  const centerY = workingHeight / 2;
  const centerX = workingWidth / 2; // Horizontal shear: each row shifts more based on distance from center
  if (finalShearX !== 0) {
    for (let y = 0; y < workingHeight; y++) {
      const distFromCenter = y - workingHeight / 2;
      const rowShift = Math.round(finalShearX * distFromCenter);

      for (let x = 0; x < workingWidth; x++) {
        let srcX = x - rowShift;
        srcX = ((srcX % workingWidth) + workingWidth) % workingWidth;

        const srcOffset = ((minY + y) * width + (minX + srcX)) * 4;
        const destOffset = ((minY + y) * width + (minX + x)) * 4;

        pixels[destOffset] = tempPixels[srcOffset];
        pixels[destOffset + 1] = tempPixels[srcOffset + 1];
        pixels[destOffset + 2] = tempPixels[srcOffset + 2];
        pixels[destOffset + 3] = tempPixels[srcOffset + 3];
      }
    }
    tempPixels.set(pixels);
  } // Vertical shear: each column shifts more based on distance from center
  if (finalShearY !== 0) {
    for (let x = 0; x < workingWidth; x++) {
      const distFromCenter = x - workingWidth / 2;
      const colShift = Math.round(finalShearY * distFromCenter);

      for (let y = 0; y < workingHeight; y++) {
        let srcY = y - colShift;
        srcY = ((srcY % workingHeight) + workingHeight) % workingHeight;

        const srcOffset = ((minY + srcY) * width + (minX + x)) * 4;
        const destOffset = ((minY + y) * width + (minX + x)) * 4;

        pixels[destOffset] = tempPixels[srcOffset];
        pixels[destOffset + 1] = tempPixels[srcOffset + 1];
        pixels[destOffset + 2] = tempPixels[srcOffset + 2];
        pixels[destOffset + 3] = tempPixels[srcOffset + 3];
      }
    }
  }
}

// b. Geometric Abstractions

// For producing a projection matrix.
// For matrix & linear algebra help: https://www.youtube.com/playlist?list=PLZHQObOWTQDPD3MizzM2xVFitgF8hE_ab
class Camera {
  type = "perspective";
  matrix;
  #x = 0;
  #rotX = 0;
  #y = 0;
  #rotY = 0;
  #z = 0;
  #rotZ = 0;
  fov;

  near = 0.001;
  far = 1000;

  position = [0, 0, 0, 1];
  rotation = [0, 0, 0];
  scale = [1, 1, 1];

  // centerCached; // Saved after each call to `center()`.

  perspectiveMatrix;
  #transformMatrix;

  // Takes x, y, z position and an optional scale (xyz) array.
  constructor(fov = 80, { x, y, z, scale } = { x: 0, y: 0, z: 0, scale: 1 }) {
    this.fov = fov;

    this.x = x;
    this.y = y;
    this.z = z;

    if (scale) this.scale = scale;

    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
  }

  set rotX(n) {
    this.#rotX = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.rotation[0] = n;
  }

  get rotX() {
    return this.#rotX;
  }

  set rotY(n) {
    this.#rotY = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.rotation[1] = n;
  }

  get rotY() {
    return this.#rotY;
  }

  set rotZ(n) {
    this.#rotZ = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.rotation[2] = n;
  }

  get rotZ() {
    return this.#rotZ;
  }

  // Returns the rotation of the camera in radians.
  get rot() {
    return [this.#rotX, this.#rotY, this.#rotZ];
  }

  set x(n = 0) {
    this.#x = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.position[0] = n;
  }

  get x() {
    return this.#x;
  }

  set y(n = 0) {
    this.#y = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.position[1] = n;
  }

  get y() {
    return this.#y;
  }

  set z(n = 0) {
    this.#z = n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
    this.position[2] = n;
  }

  get z() {
    return this.#z;
  }

  forward(n) {
    this.#z -= n;
    this.#perspective(this.fov);
    this.#transform();
    this.matrix = this.#transformMatrix;
  }

  #perspective(fov) {
    const zNear = this.near;
    const zFar = this.far;

    this.perspectiveMatrix = mat4.perspective(
      mat4.create(),
      radians(fov),
      width / height,
      zNear,
      zFar,
    );

    // See: https://github.com/BennyQBD/3DSoftwareRenderer/blob/641f59125351d9565e744a90ad86256c3970a724/src/Matrix4f.java#L89
    // And compare it with: https://glmatrix.net/docs/mat4.js.html#line1508

    const zRange = zNear - zFar;
    const ten = (-zNear - zFar) / zRange;
    const fourteen = (2 * zFar * zNear) / zRange;

    this.perspectiveMatrix[10] = ten; // Zero the Z component.
    this.perspectiveMatrix[14] = fourteen;
    this.perspectiveMatrix[11] = 1; // Flip the Y so we see things rightside up.
  }

  get perspective() {
    return this.perspectiveMatrix;
  }

  // Recalculate the camera matrix for a new display constraint.
  // TODO: Eventually this should be redundant for custom cameras
  //       that don't hook into the `screen`. 24.02.21.15.26
  resize() {
    this.forward(0);
  }

  // Get an XYZ position on a plane at a given depth,
  // relative to screen coordinates.
  ray(X = width / 2, Y = height / 2, depth = 1, flippedY = false) {
    this.#perspective(this.fov);

    // 1. Camera World Space
    const pos = [...this.position];

    if (flippedY) pos[1] *= -1; // TODO: This is a little janky now, both CPU
    //                                   and GPU should have the same Y.

    const rotX = mat4.fromXRotation(mat4.create(), radians(this.#rotX));
    const rotY = mat4.fromYRotation(mat4.create(), radians(this.#rotY));
    const rotZ = mat4.fromZRotation(mat4.create(), radians(this.#rotZ));

    const rotatedX = mat4.multiply(mat4.create(), rotX, mat4.create());
    const rotatedY = mat4.multiply(mat4.create(), rotY, rotatedX);
    const rotatedZ = mat4.multiply(mat4.create(), rotatedY, rotZ);

    const scaled = mat4.scale(mat4.create(), rotatedZ, this.scale);

    const world = scaled;

    // Camera World Space -> Inverted Perspective Projection
    const invertedProjection = mat4.invert(
      mat4.create(),
      this.perspectiveMatrix,
    );
    const invWorldPersProj = mat4.mul(mat4.create(), world, invertedProjection);

    // 2. Screen Point -> Inverted World Perspective Projection

    // Normalize from screen coordinates.
    X = 1 - X / width;
    Y = 1 - Y / height;

    // 2. -> Normalized Device Space (NDS)
    let x = 2.0 * X - 1;
    let y = 2.0 * Y - 1;

    // NDS -> Homogeneous Space
    // (flipped Z, because we are in a left-handed coordinate system.)
    const screenPos = vec4.fromValues(x, -y, 1, 1);

    // Adjust Z depth of plane... (scale screen position)
    const shiftedScreenPos = vec4.scale(vec4.create(), screenPos, depth);

    // Get near plane.
    const xyz = vec4.transformMat4(
      vec4.create(),
      shiftedScreenPos,
      invWorldPersProj,
    );

    // Subtract transformed point from camera position.
    const worldPos = vec4.sub(vec4.create(), pos, xyz);

    return worldPos;
  }

  #transform() {
    // TODO: Why does this and the FPS camera control need to be inverted?
    //       Can't I just somehow invert the matrix to avoid all the swapping?
    //       Maybe it has something to do with rotation order?
    //       The default in three.js is XYZ, but here I'm using YXZ which I
    //       had to override there. 22.10.09.20.31

    // Translation.
    const panned = mat4.translate(mat4.create(), mat4.create(), [
      this.#x,
      this.#y,
      this.#z,
    ]);

    // Rotation
    const rotY = mat4.fromYRotation(mat4.create(), radians(-this.#rotY)); // FLIPPED
    const rotX = mat4.fromXRotation(mat4.create(), radians(this.#rotX));
    const rotZ = mat4.fromZRotation(mat4.create(), radians(this.#rotZ));
    const rotatedY = mat4.multiply(mat4.create(), rotY, panned);
    const rotatedX = mat4.multiply(mat4.create(), rotX, rotatedY);
    const rotatedZ = mat4.multiply(mat4.create(), rotZ, rotatedX);

    // Scale
    // TODO: Add support for camera scaling.
    const scaled = mat4.scale(mat4.create(), rotatedZ, this.scale);

    // Perspective
    this.#transformMatrix = mat4.multiply(
      mat4.create(),
      this.perspectiveMatrix,
      scaled,
    );
  }
}

// For moving a camera round over time.
// TODO: Add a track? (Rollercoaster / coast)
// TODO: Only supports sideways movement right now.
class Dolly {
  camera;

  xVel = 0;
  yVel = 0;
  zVel = 0;
  dec = 0.9;

  constructor(camera) {
    this.camera = camera;
  }

  sim() {
    this.xVel *= this.dec;
    this.yVel *= this.dec;
    this.zVel *= this.dec;

    if (abs(this.xVel) > 0) this.camera.x += this.xVel;
    if (abs(this.yVel) > 0) this.camera.y += this.yVel;
    if (abs(this.zVel) > 0) this.camera.z += this.zVel;
  }

  push({ x, y, z }) {
    // Strafe x and z.
    const xz = vec2.rotate(
      vec2.create(),
      vec2.fromValues(x, z),
      vec2.fromValues(0, 0),
      radians(-this.camera.rotY), // Take the camera Y axis for strafing.
    );

    this.xVel += xz[0] || 0;
    this.yVel += y || 0;
    this.zVel += xz[1] || 0;
  }
}

let formId = 0;

// Mesh
class Form {
  primitive = "triangle";
  type = "triangle";

  limiter = 0; // Only enabled on CPU rendered `line` at the moment. 22.11.06.18.19

  uid; // = nanoid(4); // An id to keep across threads. Takes ~4 milliseconds. 😢

  tag; // Gets sent to the GPU as a named / marked tag.
  // Currently only available on `buffered` types in `3d.mjs` 23.02.07.09.46

  // Model
  vertices = [];
  indices = [];

  // TODO: Texture and color should be optional, and perhaps based on type.
  // TODO: Should this use a parameter called shader?
  texture; // = makeBuffer(32, 32);
  color;
  colorModifier;

  // GPU Specific Params & Buffers
  gpuVerticesSent = 0;
  gpuReset = false; // Assumes this object is being recreated on the GPU.
  gpuKeep = true;
  gpuConvertColors = true;
  gpuTransformed = false;
  gpuRecolored = false;
  MAX_POINTS = 100000; // Some buffered geometry gpu calls may use this hint.
  uvs = [];

  #gradientColors = [
    [1.0, 0.0, 0.0, 1.0],
    [0.0, 1.0, 0.0, 1.0],
    [0.0, 0.0, 1.0, 1.0],
  ];

  /* I haven't needed support for this yet so it's left commented. 22.10.13.23.12
  #texCoords = [
    [0.0, 0.0, 0.0, 0.0],
    [0.0, 1.0, 0.0, 0.0],
    [1.0, 1.0, 0.0, 0.0],
  ];
  */

  // Transform
  position = [0, 0, 0];
  rotation = [0, 0, 0];
  scale = [1, 1, 1];

  gradients = true;

  // Blending
  alpha = 1.0;

  constructor(
    // Model
    // `type` can be "triangle", or "line" or "line:buffered"
    // `positions` and colors can be sent and then verticies will be generated
    {
      type,
      vertices,
      uvs = [],
      positions,
      colors,
      gradients,
      indices,
      keep = true,
    },
    fill,
    // Transform
    transform,
  ) {
    this.gradients = gradients; // A flag to decide if we use gradients or not. Only for line3d right now. 22.11.06.02.00

    // Give an incremental id per session.
    this.uid = formId;
    formId += 1;

    // Set the primitive type.
    this.primitive = type;
    this.type = type;

    // Decide whether to throw this away after being drawn once
    this.gpuKeep = keep;

    // Take into account form -> primitive relationships.
    if (type === "quad") this.primitive = "triangle";
    if (type === "triangle:buffered") this.primitive = "triangle";
    if (type === "line:buffered") this.primitive = "line";

    this.indices = indices || repeat(positions?.length, (i) => i);

    // 🌩️ Ingest positions and turn them into vertices.
    // ("Import" a model...)

    // Switch fill to transform if the was skipped.
    if (fill?.pos || fill?.rot || fill?.scale) {
      transform = fill;
      fill = undefined;
    }

    // Assign texture or color.
    if (fill?.tex) this.texture = fill.tex;
    if (fill?.color) this.color = fill.color || c.slice();
    if (fill?.alpha) this.alpha = fill.alpha;

    // TODO: There is no maxed out notice here.
    if (positions?.length > 0)
      this.addPoints({ positions, colors }, this.indices);

    // Or just set vertices directly.
    if (vertices?.length > 0) {
      this.vertices = vertices;
      this.uvs = uvs;
    }

    this.position = transform?.pos || [0, 0, 0];
    this.rotation = transform?.rot || [0, 0, 0];

    if (typeof transform.scale === "number") {
      this.scale = [transform.scale, transform.scale, transform.scale];
    } else {
      this.scale = transform?.scale || [1, 1, 1];
    }
  }

  // TODO: This needs to support color (and eventually N vertex attributes).

  resetUID() {
    this.uid = nanoid(4);
    //this.uid = formId;
    //formId += 1;
  }

  // Clears vertex and index attributes to prepare for replacement geometry.
  clear() {
    this.uvs = [];
    this.vertices = [];
    this.indices = [];
    this.gpuReset = true;
    this.gpuVerticesSent = 0;
  }

  // How close we are to being beyond the max points allotted by the GPU for
  // buffer geometries.
  maxProgress() {
    return this.vertices.length / (this.MAX_POINTS + 1);
  }

  addPoints(attributes, indices) {
    const incomingLength = attributes.positions.length;
    const verticesLength = this.vertices.length;
    const pointsAvailable = this.MAX_POINTS - verticesLength;

    let end = incomingLength;
    let maxedOut = false;

    /* Left for debugging. 22.10.30.18.30
    if (this.MAX_POINTS === 256) {
      console.log(
        "Incoming:", incomingLength, "Current:", verticesLength,
        "Max:", this.MAX_POINTS
      );
    }
    */

    if (pointsAvailable < incomingLength) {
      end = pointsAvailable;
      maxedOut = true;
      if (debug)
        console.warn(
          "Max. cutoff in GPU form!",
          this,
          incomingLength,
          pointsAvailable,
        );
    }

    // Create new vertices from incoming positions.
    for (let i = 0; i < end; i++) {
      // Generate texCoord from position instead of loading.
      // (Vertex / 2) + 0.5 // Vertex to UV
      // See also: (Vertex - 0.5) * 2 // UV to Vertex
      // TODO: This only works for quads right now.
      const texCoord = [
        attributes.positions[i][X] / 2 + 0.5,
        attributes.positions[i][Y] / 2 + 0.5,
        //0, //positions[i][Z] / 2 + 0.5; // TODO: Is this necessary to calculate for UV?
        //0,
      ];

      // 🔥
      // TODO:
      // Wrap based on MAX_POINTS.

      this.uvs.push(...texCoord); // For sending to the GPU.

      // Optionally put color through a special function here.
      if (attributes.colors?.[i] && typeof this.colorModifier === "function") {
        attributes.colors[i] = this.colorModifier(attributes.colors[i]);
      }

      this.vertices.push(
        // For sending to the CPU.
        new Vertex(
          attributes.positions[i],
          attributes.colors?.[i],
          // this.#gradientColors[i % 3],
          texCoord, //this.#texCoords[i % 3] // Replace to enable bespoke texture coordinates.
          attributes.normals?.[i],
        ),
      );

      // TODO: This may need to be turned back on for the GPU?
      //       What was with the -1 here?  22.11.06.17.42
      // if (!indices) this.indices.push(verticesLength - 1 + i);
      if (!indices) this.indices.push(verticesLength + i);
      // console.log(indices, !indices, i, verticesLength);
    }

    if (indices) this.indices = indices;

    // Create indices from pre-indexed positions or generate
    // a linear set of indices based on length.

    // TODO: How inefficient is this? 22.10.30.17.30
    // this.indices = indices || repeat(this.vertices.length, (i) => i);

    return maxedOut;
  }

  // Get the world position of this form's local vertex.
  transformVertex(vertex) {
    // Build a matrix to represent this form's position, rotation and scale.
    const panned = mat4.fromTranslation(mat4.create(), [
      this.position[X],
      this.position[Y],
      this.position[Z],
    ]);

    const rotX = mat4.fromXRotation(mat4.create(), radians(this.rotation[X]));
    const rotY = mat4.fromYRotation(mat4.create(), radians(this.rotation[Y]));
    const rotZ = mat4.fromZRotation(mat4.create(), radians(this.rotation[Z]));

    const rotatedX = mat4.mul(mat4.create(), panned, rotX);
    const rotatedY = mat4.mul(mat4.create(), rotatedX, rotY);
    const rotatedZ = mat4.mul(mat4.create(), rotatedY, rotZ);

    const matrix = rotatedZ;

    //mat4.translate(matrix, matrix, this.position);

    // Apply scale.
    mat4.scale(matrix, matrix, this.scale);

    // Apply the world matrix.
    //matrix = mat4.mul(mat4.create(), worldMatrix, matrix);

    // const transformedVertices = [];
    // Transform each vertex by the matrix.
    //this.vertices.forEach((vertex) => {
    return vertex.transformWorld(matrix);
    //});
  }
  graph({ matrix: cameraMatrix }) {
    // Build a matrix to represent this form's position, rotation and scale.
    const panned = mat4.fromTranslation(mat4.create(), [
      this.position[X] * -1,
      this.position[Y],
      this.position[Z] * -1,
    ]);

    const rotX = mat4.fromXRotation(mat4.create(), radians(this.rotation[X]));
    const rotY = mat4.fromYRotation(mat4.create(), radians(this.rotation[Y]));
    const rotZ = mat4.fromZRotation(mat4.create(), radians(this.rotation[Z]));

    const rotatedX = mat4.multiply(mat4.create(), rotX, panned);
    const rotatedY = mat4.multiply(mat4.create(), rotY, rotatedX);
    const rotatedZ = mat4.multiply(mat4.create(), rotZ, rotatedY);

    // Scale
    const scaled = mat4.scale(mat4.create(), rotatedZ, this.scale); // Render wireframe lines for line type forms using untransformed vertices
    if (this.type === "line" && this.vertices.length > 0) {
      const lineColor = this.color || [255, 0, 0, 255]; // Default to red

      // Use the full combined matrix for line3d
      const fullMatrix = mat4.multiply(mat4.create(), cameraMatrix, scaled);

      // Render lines between pairs of vertices using original vertices
      for (let i = 0; i < this.vertices.length; i += 2) {
        if (i + 1 < this.vertices.length) {
          const a = this.vertices[i];
          const b = this.vertices[i + 1];
          // Create temporary transformed vertices for line3d
          const transformedA = a.transform(fullMatrix);
          const transformedB = b.transform(fullMatrix);

          // Skip drawing if either vertex is behind the camera (negative Z)
          if (transformedA.pos[2] <= 0 || transformedB.pos[2] <= 0) {
            continue;
          }

          // Apply perspective divide and screen space transformation
          const perspA = perspectiveDivide(transformedA);
          const perspB = perspectiveDivide(transformedB);
          const screenA = toScreenSpace(perspA);
          const screenB = toScreenSpace(perspB);

          // Draw simple 2D line
          line(screenA.pos[0], screenA.pos[1], screenB.pos[0], screenB.pos[1]);
        }
      }
    }

    // Still return transformed vertices for compatibility
    const transformedVertices = [];
    const matrix = mat4.multiply(mat4.create(), cameraMatrix, scaled);
    this.vertices.forEach((vertex) => {
      transformedVertices.push(vertex.transform(matrix));
    });

    return transformedVertices;
  }
}

// Constants for accessing Vector / Position components.
const X = 0;
const Y = 1;
const Z = 2;
const W = 3;

// A single point in space with color and texture coordinate information.
class Vertex {
  static X = 0;
  static Y = 1;
  static Z = 2;
  static W = 3;

  pos;
  color;
  texCoords;
  normal;
  constructor(
    position = [0, 0, 0, 1],
    color = [1, 1, 1, 1],
    textureCoordinates = [0, 0],
    normal = null,
  ) {
    this.pos = position;
    this.color = color;
    this.texCoords = textureCoordinates;

    if (normal !== null) {
      this.normal = vec3.fromValues(...normal);
    } else {
      this.normal = vec3.fromValues(0, 0, 1); // Default normal pointing up
    }
  }

  // TODO: Optimize this function for large vertex counts. 22.10.13.00.14
  transform(matrix) {
    // Camera
    const vert = new Vertex(
      vec4.transformMat4(
        vec4.create(),
        [
          this.pos[X] * -1, // FLIPPED
          this.pos[Y],
          this.pos[Z] * -1, // FLIPPED
          this.pos[W],
        ],
        matrix,
      ),
      this.color,
      this.texCoords,
    );
    // console.log(matrix, vert);
    return vert;
  }

  transformWorld(matrix) {
    return new Vertex(
      vec4.transformMat4(vec4.create(), this.pos, matrix),
      this.color,
      this.texCoords,
    );
  }
}

// Sutherland-Hodgman clipping algorithm
function clip(vertices, clippingBoundary) {
  let clipped = [];

  function inside(p, edge) {
    switch (edge) {
      case "left":
        return p[X] >= -1;
      case "right":
        return p[X] <= 1;
      case "bottom":
        return p[Y] >= -1;
      case "top":
        return p[Y] <= 1;
      case "near":
        return p[Z] >= 0;
      case "far":
        return p[Z] <= 1;
    }
  }

  function computeIntersection(p1, p2, edge) {
    let t;
    switch (edge) {
      case "left":
        t = (-1 - p1[X]) / (p2[X] - p1[X]);
        break;
      case "right":
        t = (1 - p1[X]) / (p2[X] - p1[X]);
        break;
      case "bottom":
        t = (-1 - p1[Y]) / (p2[Y] - p1[Y]);
        break;
      case "top":
        t = (1 - p1[Y]) / (p2[Y] - p1[Y]);
        break;
      case "near":
        t = (0 - p1[Z]) / (p2[Z] - p1[Z]);
        break;
      case "far":
        t = (1 - p1[Z]) / (p2[Z] - p1[Z]);
        break;
    }

    return vec4.lerp(vec4.create(), p1, p2, t);
  }

  for (const edge of clippingBoundary) {
    const input = clipped.length > 0 ? clipped : vertices;
    clipped = [];

    if (input.length === 0) break;

    let prevVertex = input[input.length - 1];

    for (let i = 0; i < input.length; i++) {
      const curVertex = input[i];

      if (inside(curVertex.pos, edge)) {
        if (!inside(prevVertex.pos, edge)) {
          const intersection = computeIntersection(
            prevVertex.pos,
            curVertex.pos,
            edge,
          );
          clipped.push(new Vertex(intersection, curVertex.color));
        }
        clipped.push(curVertex);
      } else if (inside(prevVertex.pos, edge)) {
        const intersection = computeIntersection(
          prevVertex.pos,
          curVertex.pos,
          edge,
        );
        clipped.push(new Vertex(intersection, curVertex.color));
      }

      prevVertex = curVertex;
    }
  }

  return clipped;
}

// for 3d line clipping
function clipLineToFrustum(v1, v2) {
  const clippingBoundary = ["left", "right", "bottom", "top", "near", "far"];

  function inside(p, edge) {
    switch (edge) {
      case "left":
        return p[X] >= -p[W];
      case "right":
        return p[X] <= p[W];
      case "bottom":
        return p[Y] >= -p[W];
      case "top":
        return p[Y] <= p[W];
      case "near":
        return p[Z] >= 0;
      case "far":
        return p[Z] <= p[W];
    }
  }

  function computeIntersection(p1, p2, edge) {
    let t;
    switch (edge) {
      case "left":
        t = (-p1[W] - p1[X]) / (p2[X] - p1[X] + p2[W] - p1[W]);
        break;
      case "right":
        t = (p1[W] - p1[X]) / (p2[X] - p1[X] - p2[W] + p1[W]);
        break;
      case "bottom":
        t = (-p1[W] - p1[Y]) / (p2[Y] - p1[Y] + p2[W] - p1[W]);
        break;
      case "top":
        t = (p1[W] - p1[Y]) / (p2[Y] - p1[Y] - p2[W] + p1[W]);
        break;
      case "near":
        t = (0 - p1[Z]) / (p2[Z] - p1[Z]);
        break;
      case "far":
        t = (p1[W] - p1[Z]) / (p2[Z] - p1[Z] - p2[W] + p1[W]);
        break;
    }

    return [
      p1[X] + t * (p2[X] - p1[X]),
      p1[Y] + t * (p2[Y] - p1[Y]),
      p1[Z] + t * (p2[Z] - p1[Z]),
      p1[W] + t * (p2[W] - p1[W]),
    ];
  }

  let clippedVertices = [v1, v2];

  for (const edge of clippingBoundary) {
    if (clippedVertices.length < 2) break;

    const [p1, p2] = clippedVertices;
    const p1Inside = inside(p1.pos, edge);
    const p2Inside = inside(p2.pos, edge);

    if (p1Inside && p2Inside) {
      // Both inside, keep both
      continue;
    } else if (!p1Inside && !p2Inside) {
      // Both outside, discard both
      clippedVertices = [];
      break;
    } else {
      // One inside, one outside
      const intersection = computeIntersection(p1.pos, p2.pos, edge);
      const intersectionVertex = new Vertex(intersection, p1.color);

      if (p1Inside) {
        // p1 inside, p2 outside
        clippedVertices = [p1, intersectionVertex];
      } else {
        // p1 outside, p2 inside
        clippedVertices = [intersectionVertex, p2];
      }
    }
  }

  return clippedVertices;
}

function perspectiveDivide(vertex) {
  const vert = new Vertex([
    vertex.pos[X] / vertex.pos[W],
    vertex.pos[Y] / vertex.pos[W],
    vertex.pos[Z] / vertex.pos[W],
    vertex.pos[W],
  ]);

  vert.color = vertex.color;
  vert.texCoords = vertex.texCoords;

  return vert;
}

function toScreenSpace(vertex) {
  // Flip Y.
  const x = vertex.pos[X];
  const y = -vertex.pos[Y];
  const z = vertex.pos[Z];

  const sX = ((x + 1.0) / 2.0) * width;
  const sY = ((y + 1.0) / 2.0) * height;

  const vert = new Vertex([sX, sY, z, vertex.pos[W]]);
  vert.color = vertex.color;
  vert.texCoords = vertex.texCoords;
  return vert;
}

function zeroLineClip(vertices) {
  const clipped = [];

  let prevVertex = vertices[vertices.length - 1]; // Start with the last vertex
  let prevComponent = prevVertex.pos[Z];
  let prevInside = prevComponent >= 0;

  for (const curVertex of vertices) {
    const curComponent = curVertex.pos[Z];
    const curInside = curComponent >= 0;

    if (curInside !== prevInside) {
      // Edge crosses the z=0 plane, find intersection
      const t = prevComponent / (prevComponent - curComponent);
      const intersectionPos = [
        prevVertex.pos[X] + t * (curVertex.pos[X] - prevVertex.pos[X]),
        prevVertex.pos[Y] + t * (curVertex.pos[Y] - prevVertex.pos[Y]),
        0, // z = 0
        prevVertex.pos[W] + t * (curVertex.pos[W] - prevVertex.pos[W]),
      ];

      // Interpolate color as well
      const intersectionColor = [
        prevVertex.color[0] + t * (curVertex.color[0] - prevVertex.color[0]),
        prevVertex.color[1] + t * (curVertex.color[1] - prevVertex.color[1]),
        prevVertex.color[2] + t * (curVertex.color[2] - prevVertex.color[2]),
        prevVertex.color[3] + t * (curVertex.color[3] - prevVertex.color[3]),
      ];

      clipped.push(new Vertex(intersectionPos, intersectionColor));
    }

    if (curInside) {
      clipped.push(curVertex);
    }

    prevVertex = curVertex;
    prevComponent = curComponent;
    prevInside = curInside;
  }
}

// Function to set KidLisp evaluation context for dynamic fade directions
function setKidLispContext(kidlispInstance, api, env) {
  currentKidLispContext = {
    evaluate: kidlispInstance.evaluate.bind(kidlispInstance),
    api,
    env
  };
}

// Function to clear KidLisp evaluation context
function clearKidLispContext() {
  currentKidLispContext = null;
}

export {
  clear,
  point,
  plot,
  flood,
  colorsMatch,
  pan,
  unpan,
  savepan,
  loadpan,
  mask,
  unmask,
  skip,
  copy,
  resize,
  contrast,
  brightness,
  paste,
  stamp,
  steal,
  putback,
  line,
  pline,
  pixelPerfectPolyline,
  lineAngle,
  circle,
  oval,
  poly,
  box,
  tri,
  shape,
  grid,
  draw,
  noise16,
  noise16DIGITPAIN,
  noise16Aesthetic,
  noise16Sotce,
  noiseTinted,
  printLine,
  blendMode,
  scroll,
  resetScrollState,
  spin,
  zoom,
  suck,
  blur,
  cleanupBlurBuffers,
  sort,
  shear,
  setBlockProcessing,
  Camera,
  Form,
  Dolly,
  setKidLispContext,
  clearKidLispContext,
};
