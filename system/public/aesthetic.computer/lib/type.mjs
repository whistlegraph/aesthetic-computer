// Type, 22.12.10.14.06
// Abstraction for typography and text input.

/* #region 🏁 TODO
  + Future
  - [] Gracefully allow for multiple instances of TextInput in a single piece? 
endregion */

import * as fonts from "../disks/common/fonts.mjs";
import { repeat } from "../lib/help.mjs";
import { checkPackMode } from "./pack-mode.mjs";
import { KidLisp, tokenize } from "./kidlisp.mjs";
import { cssColors } from "./num.mjs";
import { log } from "./logs.mjs";
import { openDB } from "../dep/idb.js";

// 🗄️ Global Glyph Cache System (IndexedDB-backed)
// Provides fast glyph loading on subsequent visits
let glyphDbPromise = null;
const GLYPH_CACHE_VERSION = 2; // Increment to invalidate all cached glyphs
const GLYPH_STORE_NAME = "glyphs";
const META_STORE_NAME = "glyph-meta";

// In-memory cache for immediate access (populated from IDB on init)
const glyphMemoryCache = new Map(); // key: "fontName:char" → glyphData
let glyphCacheInitialized = false;
let glyphCacheInitPromise = null;

// Initialize the glyph cache database
async function initGlyphCache() {
  if (glyphCacheInitialized) return glyphDbPromise;
  if (glyphCacheInitPromise) return glyphCacheInitPromise;
  
  glyphCacheInitPromise = (async () => {
    try {
      glyphDbPromise = await openDB("ac-glyph-cache", GLYPH_CACHE_VERSION, {
        upgrade(db, oldVersion, newVersion) {
          // Clear old stores on version change
          if (db.objectStoreNames.contains(GLYPH_STORE_NAME)) {
            db.deleteObjectStore(GLYPH_STORE_NAME);
          }
          if (db.objectStoreNames.contains(META_STORE_NAME)) {
            db.deleteObjectStore(META_STORE_NAME);
          }
          // Create fresh stores
          db.createObjectStore(GLYPH_STORE_NAME);
          db.createObjectStore(META_STORE_NAME);
          console.log(`🔤 Glyph cache upgraded: v${oldVersion} → v${newVersion}`);
        },
      });
      glyphCacheInitialized = true;
      return glyphDbPromise;
    } catch (err) {
      console.warn("🔤 Glyph cache unavailable:", err.message);
      glyphCacheInitialized = true;
      return null;
    }
  })();
  
  return glyphCacheInitPromise;
}

// Get a glyph from cache (memory first, then IDB)
async function getCachedGlyph(fontName, char) {
  const key = `${fontName}:${char}`;
  
  // Check memory cache first (instant)
  if (glyphMemoryCache.has(key)) {
    return glyphMemoryCache.get(key);
  }
  
  // Check IndexedDB
  const db = await initGlyphCache();
  if (!db) return null;
  
  try {
    const data = await db.get(GLYPH_STORE_NAME, key);
    if (data) {
      // Populate memory cache for next access
      glyphMemoryCache.set(key, data);
      return data;
    }
  } catch (err) {
    // Silently fail - cache miss
  }
  return null;
}

// Save a glyph to both memory and IDB cache
async function cacheGlyph(fontName, char, glyphData) {
  if (!glyphData) return;
  
  const key = `${fontName}:${char}`;
  
  // Always update memory cache
  glyphMemoryCache.set(key, glyphData);
  
  // Persist to IndexedDB (non-blocking)
  const db = await initGlyphCache();
  if (!db) return;
  
  try {
    await db.put(GLYPH_STORE_NAME, glyphData, key);
  } catch (err) {
    // Silently fail - cache write error
  }
}

// Bulk save multiple glyphs (more efficient for batch loads)
async function cacheGlyphsBulk(fontName, glyphsMap) {
  if (!glyphsMap || typeof glyphsMap !== 'object') return;
  
  const db = await initGlyphCache();
  
  // Update memory cache immediately
  for (const [char, data] of Object.entries(glyphsMap)) {
    if (data) {
      glyphMemoryCache.set(`${fontName}:${char}`, data);
    }
  }
  
  if (!db) return;
  
  // Batch write to IndexedDB
  try {
    const tx = db.transaction(GLYPH_STORE_NAME, 'readwrite');
    const store = tx.objectStore(GLYPH_STORE_NAME);
    
    const promises = Object.entries(glyphsMap).map(([char, data]) => {
      if (data) {
        return store.put(data, `${fontName}:${char}`);
      }
    }).filter(Boolean);
    
    await Promise.all([...promises, tx.done]);
  } catch (err) {
    console.warn("🔤 Bulk glyph cache write failed:", err.message);
  }
}

// Pre-warm the memory cache from IndexedDB for a specific font
async function preWarmGlyphCache(fontName) {
  const db = await initGlyphCache();
  if (!db) return 0;
  
  try {
    const tx = db.transaction(GLYPH_STORE_NAME, 'readonly');
    const store = tx.objectStore(GLYPH_STORE_NAME);
    const allKeys = await store.getAllKeys();
    
    // Filter keys for this font
    const fontKeys = allKeys.filter(k => typeof k === 'string' && k.startsWith(`${fontName}:`));
    
    if (fontKeys.length === 0) return 0;
    
    // Load all glyphs for this font into memory
    const loadPromises = fontKeys.map(async (key) => {
      const data = await db.get(GLYPH_STORE_NAME, key);
      if (data) {
        glyphMemoryCache.set(key, data);
      }
    });
    
    await Promise.all(loadPromises);
    return fontKeys.length;
  } catch (err) {
    console.warn("🔤 Glyph cache pre-warm failed:", err.message);
    return 0;
  }
}

// Clear glyph cache for a specific font (useful for dev/debugging)
async function clearGlyphCache(fontName) {
  const db = await initGlyphCache();
  
  // Clear from memory
  for (const key of glyphMemoryCache.keys()) {
    if (!fontName || key.startsWith(`${fontName}:`)) {
      glyphMemoryCache.delete(key);
    }
  }
  
  if (!db) return;
  
  try {
    if (fontName) {
      // Clear only specific font
      const tx = db.transaction(GLYPH_STORE_NAME, 'readwrite');
      const store = tx.objectStore(GLYPH_STORE_NAME);
      const allKeys = await store.getAllKeys();
      
      for (const key of allKeys) {
        if (typeof key === 'string' && key.startsWith(`${fontName}:`)) {
          await store.delete(key);
        }
      }
      await tx.done;
    } else {
      // Clear all
      await db.clear(GLYPH_STORE_NAME);
    }
    console.log(`🔤 Glyph cache cleared${fontName ? ` for ${fontName}` : ''}`);
  } catch (err) {
    console.warn("🔤 Glyph cache clear failed:", err.message);
  }
}

// Export cache utilities for debugging
if (typeof window !== 'undefined') {
  window.acGlyphCache = {
    preWarm: preWarmGlyphCache,
    clear: clearGlyphCache,
    stats: () => ({
      memorySize: glyphMemoryCache.size,
      initialized: glyphCacheInitialized,
    }),
  };
}

function matrixDebugEnabled() {
  if (typeof window !== "undefined" && window?.acMatrixDebug) return true;
  if (typeof globalThis !== "undefined" && globalThis?.acMatrixDebug) return true;
  return false;
}

const { floor, min } = Math;
const { keys, entries } = Object;
const undef = undefined;

// Preloads and holds the glyphs for a system typeface.
class Typeface {
  data;
  name;
  glyphs = {};
  advanceCache = new Map();
  //loaded = false;

  constructor(name = "font_1") {
    this.name = name;
    // Use fonts.mjs as the single source of truth for all font data
    this.data = fonts[name] || fonts.font_1;
  }

  // Return only the character index from the data.
  get glyphData() {
    const glyphsOnly = { ...this.data };
    // TODO: Remove other "glyph" prefixes here if they ever exist. 23.06.07.01.10
    delete glyphsOnly.glyphHeight;
    delete glyphsOnly.glyphWidth;
    return glyphsOnly;
  }

  get blockWidth() {
    // For proportional fonts like MatrixChunky8, use a default advance width
    // since they don't have a fixed glyphWidth
    if (this.data.proportional === true || this.data.bdfFont === "MatrixChunky8" || this.name === "MatrixChunky8") {
      return 4; // Default character advance for MatrixChunky8
    }
    return this.data.glyphWidth;
  }

  get blockHeight() {
    return this.data.glyphHeight;
  }

  async load($preload, needsPaintCallback) {
    // TODO: Add support for on-demand character loading here using this api that
    //       gets the json for the glyphs: https://localhost:8888/api/bdf-glyph?char=h
    if (this.name === "font_1") {
      // Try to pre-warm from IndexedDB cache first
      const cachedCount = await preWarmGlyphCache(this.name);
      if (cachedCount > 0) {
        // Populate glyphs from memory cache
        for (const [key, data] of glyphMemoryCache.entries()) {
          if (key.startsWith(`${this.name}:`)) {
            const char = key.slice(this.name.length + 1);
            this.glyphs[char] = data;
          }
        }
      }
      
      // 1. Ignore any keys with a "glyph" prefix because these are settings.
      const glyphsToLoad = entries(this.data).filter(
        ([g, loc]) => !g.startsWith("glyph") && typeof loc === "string" && loc !== "false" && loc.length > 0,
      );
      
      // Filter out already-cached glyphs
      const glyphsNeedingFetch = glyphsToLoad.filter(([glyph]) => !this.glyphs[glyph]);
      
      const glyphsToCache = {};
      const promises = glyphsNeedingFetch.map(([glyph, location], i) => {
        // 2. Load all other keys / glyphs over the network.
        return $preload(
          `aesthetic.computer/disks/drawings/${this.name}/${location}.json`,
        )
          .then((res) => {
            this.glyphs[glyph] = res;
            glyphsToCache[glyph] = res;
          })
          .catch((err) => {
            // Silently handle missing glyph files - some glyphs may not exist
            // console.error("Couldn't load typeface:", err);
          });
      }); // Wait for all the promises to resolve before returning
      await Promise.all(promises);
      
      // Cache newly loaded glyphs to IndexedDB
      if (Object.keys(glyphsToCache).length > 0) {
        cacheGlyphsBulk(this.name, glyphsToCache);
      }
    } else if (this.name === "microtype") {
      // Try to pre-warm from IndexedDB cache first
      const cachedCount = await preWarmGlyphCache(this.name);
      if (cachedCount > 0) {
        for (const [key, data] of glyphMemoryCache.entries()) {
          if (key.startsWith(`${this.name}:`)) {
            const char = key.slice(this.name.length + 1);
            this.glyphs[char] = data;
          }
        }
      }
      
      // Load microtype 3x5 font
      const glyphsToLoad = entries(this.data).filter(
        ([g, loc]) => !g.startsWith("glyph") && typeof loc === "string" && loc !== "false" && loc.length > 0,
      );
      
      // Filter out already-cached glyphs
      const glyphsNeedingFetch = glyphsToLoad.filter(([glyph]) => !this.glyphs[glyph]);
      
      const glyphsToCache = {};
      const promises = glyphsNeedingFetch.map(([glyph, location], i) => {
        const path = `aesthetic.computer/disks/drawings/${location}.json`;
        return $preload(path)
          .then((res) => {
            this.glyphs[glyph] = res;
            glyphsToCache[glyph] = res;
          })
          .catch((err) => {
            console.error(`❌ Couldn't load microtype glyph "${glyph}":`, err);
          });
      });
      await Promise.all(promises);
      
      // Cache newly loaded glyphs
      if (Object.keys(glyphsToCache).length > 0) {
        cacheGlyphsBulk(this.name, glyphsToCache);
      }
    } else if (this.name === "unifont" || this.data.bdfFont) {
      // 🗺️ BDF Font support - includes UNIFONT and other BDF fonts
      // Determine which font to use - for unifont use the name, for others use bdfFont property
      const fontName = this.data.bdfFont || "unifont";
      
      // Check if we're in OBJKT mode and this is MatrixChunky8
      const { checkPackMode } = await import("./pack-mode.mjs");
      const isObjktMode = checkPackMode();
      
      if (isObjktMode && this.name === "MatrixChunky8") {
        // Set essential font metadata properties for proportional font detection
        this.data.proportional = true;
        this.data.bdfFont = "MatrixChunky8";
        this.data.name = "MatrixChunky8";
        // Import advance values from fonts.mjs for fallback
        const { MatrixChunky8 } = await import("../disks/common/fonts.mjs");
        this.data.advances = MatrixChunky8.advances || {}; // Character advance widths from fonts.mjs
        this.data.bdfOverrides = MatrixChunky8.bdfOverrides || {}; // Position overrides from fonts.mjs

        const inlineGlyphMap =
          (typeof globalThis !== "undefined" && globalThis.acOBJKT_MATRIX_CHUNKY_GLYPHS) ||
          (typeof window !== "undefined" && window.acOBJKT_MATRIX_CHUNKY_GLYPHS) ||
          null;

        // Stash the inline glyph map for later on-demand lookups
        if (inlineGlyphMap && typeof inlineGlyphMap === "object") {
          if (!this.data || typeof this.data !== "object") {
            this.data = {};
          }
          this.inlineMatrixChunkyGlyphMap = inlineGlyphMap;
          this.data.inlineMatrixChunkyGlyphMap = inlineGlyphMap;
        }
        
        // Define the characters we want to preload from the bundled assets using hex codes
        const chars = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
                      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
                      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                      ' ', '.', ',', '!', '?', ':', ';', '-', '+', '=', '<', '>', '/', '\\', '|', '"', "'", '(', ')', '[', ']', '{', '}', '@', '#', '$', '%', '^', '&', '*', '_', '~'];
        
        // Load glyph data from bundled assets using hex-encoded filenames
        const promises = chars.map(async (char) => {
          try {
            // Convert character to hex code for filename
            const charCode = char.charCodeAt(0);
            const hexCode = charCode.toString(16).toUpperCase().padStart(4, '0');

            const glyphData = inlineGlyphMap?.[hexCode];
            if (!glyphData) {
              return null;
            }
            this.data[char] = glyphData; // Store in font data for proxy access
            this.invalidateAdvance(char);
            return glyphData;
          } catch (err) {
            return null;
          }
        });
        
        await Promise.all(promises);
      }
      
      // Add MatrixChunky8 question mark glyph (from BDF: ENCODING 63)
      // BBX 3 7 0 1 = 3px wide, 7px tall, xOffset=0, yOffset=1
      // DWIDTH 4 0 = advance width 4px
      this.glyphs["?"] = {
        resolution: [3, 7],
        offset: [0, 1],
        baselineOffset: [0, 0],
        advance: 4,
        pixels: [
          [1, 1, 1], // E0 = 11100000
          [1, 0, 1], // A0 = 10100000
          [0, 0, 1], // 20 = 00100000
          [0, 1, 1], // 60 = 01100000
          [0, 1, 0], // 40 = 01000000
          [0, 0, 0], // 00 = 00000000
          [0, 1, 0], // 40 = 01000000
        ],
      };

      // Add a better fallback for emoji - a simple smiley
      this.glyphs["☺"] = {
        resolution: [6, 9],
        pixels: [
          [0, 1, 1, 1, 1, 0],
          [1, 0, 0, 0, 0, 1],
          [1, 0, 1, 0, 1, 0],
          [1, 0, 0, 0, 0, 1],
          [1, 0, 1, 1, 0, 1],
          [1, 0, 0, 0, 0, 1],
          [0, 1, 1, 1, 1, 0],
          [0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0],
        ],
      };

      // Create a set to track glyphs currently being loaded to avoid duplicate requests
      const loadingGlyphs = new Set();

      // Create a set to track failed glyphs to avoid repeated requests
      const failedGlyphs = new Set();

      // Batch queue for glyph requests - collects characters and flushes periodically
      const batchQueue = [];
      let batchTimeout = null;
      const BATCH_DELAY = 16; // ms to wait before flushing batch (roughly 1 frame)
      const MAX_BATCH_SIZE = 50; // Max characters per batch request
      
      const flushBatch = async () => {
        if (batchQueue.length === 0) return;
        
        // Take items from queue
        const batch = batchQueue.splice(0, MAX_BATCH_SIZE);
        batchTimeout = null;
        
        // Build comma-separated code points for batch API
        // Filter out empty or invalid code point strings
        const codePointStrs = batch
          .map(item => item.codePointStr)
          .filter(s => s && s.length > 0 && /^[0-9A-F_]+$/i.test(s));
        
        // Skip if nothing valid to fetch
        if (codePointStrs.length === 0) {
          console.warn("Batch glyph fetch skipped: no valid code points. Raw batch:", batch.map(b => ({ char: b.char, str: b.codePointStr })));
          for (const item of batch) {
            failedGlyphs.add(item.char);
            loadingGlyphs.delete(item.char);
          }
          return;
        }
        
        // Debug log the actual URL being constructed
        const charsParam = codePointStrs.join(',');
        log.net.verbose(`Batch glyph fetch: ${codePointStrs.length} chars, font=${this.name}`);
        
        const batchStart = performance.now();
        try {
          const apiUrl = (typeof window !== 'undefined' && window.acSPIDER)
            ? `https://aesthetic.computer/api/bdf-glyph?chars=${charsParam}&font=${this.name}`
            : `/api/bdf-glyph?chars=${charsParam}&font=${this.name}`;
          
          const response = await fetch(apiUrl);
          if (!response.ok) {
            const errorText = await response.text();
            console.error(`🔤 Batch glyph API error (${response.status}):`, errorText, `URL: ${apiUrl}`);
            throw new Error(`HTTP ${response.status}`);
          }
          
          const data = await response.json();
          const glyphs = data.glyphs || {};
          const batchDuration = performance.now() - batchStart;
          
          // Process results and cache them
          const glyphsToCache = {};
          for (const item of batch) {
            const glyphData = glyphs[item.codePointStr];
            if (glyphData) {
              item.target[item.char] = glyphData;
              glyphsToCache[item.char] = glyphData;
              this.invalidateAdvance(item.char);
              if (needsPaintCallback && typeof needsPaintCallback === "function") {
                needsPaintCallback();
              }
            } else {
              failedGlyphs.add(item.char);
            }
            loadingGlyphs.delete(item.char);
          }
          
          // Bulk cache all fetched glyphs to IndexedDB (non-blocking)
          if (Object.keys(glyphsToCache).length > 0) {
            cacheGlyphsBulk(this.name, glyphsToCache);
          }
        } catch (err) {
          console.warn(`Batch glyph fetch failed:`, err);
          // Mark all as failed
          for (const item of batch) {
            failedGlyphs.add(item.char);
            loadingGlyphs.delete(item.char);
          }
        }
        
        // If more items were added while fetching, flush again
        if (batchQueue.length > 0 && !batchTimeout) {
          batchTimeout = setTimeout(flushBatch, BATCH_DELAY);
        }
      };
      
      const queueGlyphFetch = (char, codePointStr, target) => {
        batchQueue.push({ char, codePointStr, target });
        
        // Flush immediately if we hit max batch size
        if (batchQueue.length >= MAX_BATCH_SIZE) {
          if (batchTimeout) {
            clearTimeout(batchTimeout);
            batchTimeout = null;
          }
          flushBatch();
        } else if (!batchTimeout) {
          // Schedule a flush after a short delay to collect more requests
          batchTimeout = setTimeout(flushBatch, BATCH_DELAY);
        }
      };
      
      // Pre-warm glyph cache from IndexedDB for this font (non-blocking)
      preWarmGlyphCache(this.name).then(count => {
        if (count > 0) {
          // Populate glyphs object from memory cache
          for (const [key, data] of glyphMemoryCache.entries()) {
            if (key.startsWith(`${this.name}:`)) {
              const char = key.slice(this.name.length + 1);
              if (!this.glyphs[char]) {
                // Directly set on underlying target to avoid proxy
                Object.defineProperty(this.glyphs, char, {
                  value: data,
                  writable: true,
                  enumerable: true,
                  configurable: true,
                });
              }
            }
          }
          // Trigger repaint if callback available
          if (needsPaintCallback && typeof needsPaintCallback === "function") {
            needsPaintCallback();
          }
        }
      });

      // Wrap the glyphs object with a Proxy for automatic loading
      this.glyphs = new Proxy(this.glyphs, {
        get: (target, char) => {
          // if (char === "glyphWidth") console.log("Target:", target, "Char:", char);

          // If glyph exists, return it immediately
          if (target[char]) {
            return target[char];
          }
          
          // Check memory cache (populated from IndexedDB pre-warm)
          const cacheKey = `${this.name}:${char}`;
          if (glyphMemoryCache.has(cacheKey)) {
            const cached = glyphMemoryCache.get(cacheKey);
            target[char] = cached;
            return cached;
          }

          // If glyph has failed to load before, don't try again
          if (failedGlyphs.has(char)) {
            return this.getEmojiFallback(char, target);
          }

          // If glyph is currently being loaded, return placeholder
          if (loadingGlyphs.has(char)) {
            return this.getEmojiFallback(char, target);
          }

          // Skip invalid characters (but allow "?" to load normally)
          if (typeof char !== "string" || char.length === 0) {
            return null;
          }

          // Start loading the glyph asynchronously
          loadingGlyphs.add(char);

          // Convert character to Unicode code point(s) for API request
          // This handles emoji and multi-byte characters properly
          const codePoints = [];

          // Handle the case where we might have invalid/corrupted characters
          try {
            // Use Array.from to properly iterate over Unicode code points
            // This handles surrogate pairs correctly
            const characters = Array.from(char);

            for (const singleChar of characters) {
              const codePoint = singleChar.codePointAt(0);

              if (codePoint !== undefined) {
                // Check for lone surrogates (U+D800-U+DFFF) which are invalid
                if (codePoint >= 0xd800 && codePoint <= 0xdfff) {
                  console.warn(
                    `Invalid lone surrogate detected: U+${codePoint.toString(16).toUpperCase()} in char "${char}"`,
                  );
                  // For debugging, let's also try to reconstruct what this should be
                  if (char.length >= 2) {
                    console.warn(
                      `Original char length: ${char.length}, char codes:`,
                      Array.from(char).map(
                        (c) =>
                          `U+${c.codePointAt(0).toString(16).toUpperCase()}`,
                      ),
                    );
                  }
                  // Use replacement character instead
                  codePoints.push("FFFD");
                  continue;
                }

                // Use proper hex formatting for all code points
                const hexValue = codePoint
                  .toString(16)
                  .toUpperCase()
                  .padStart(codePoint > 0xffff ? 5 : 4, "0");
                codePoints.push(hexValue);
              }
            }
          } catch (error) {
            console.warn(`Error processing character "${char}":`, error);
            // Fallback to replacement character
            codePoints.push("FFFD");
          }

          // If no valid code points were found, use replacement character
          if (codePoints.length === 0) {
            codePoints.push("FFFD");
          }

          // Join multiple code points with underscores for complex characters
          const codePointStr = codePoints.join("_");

          // Make API call to load the glyph using code points
          const isObjktMode = checkPackMode();
          
          if (isObjktMode && this.name === "MatrixChunky8") {
            // In OBJKT mode, first check if glyph is directly on target
            if (target[char] && target[char].pixels && target[char].resolution) {
              loadingGlyphs.delete(char);
              return target[char];
            }

            // In OBJKT mode, the bundled data might be in the font object's data
            // Access through this.data (the original font data object)
            if (this.data && this.data[char]) {
              // Store it in target for faster access next time
              target[char] = this.data[char];
              loadingGlyphs.delete(char);
              return this.data[char];
            }

            const inlineGlyphMap =
              this.inlineMatrixChunkyGlyphMap ||
              this.data?.inlineMatrixChunkyGlyphMap ||
              (typeof globalThis !== "undefined" && globalThis.acOBJKT_MATRIX_CHUNKY_GLYPHS) ||
              (typeof window !== "undefined" && window.acOBJKT_MATRIX_CHUNKY_GLYPHS) ||
              null;

            if (inlineGlyphMap && codePoints.length === 1) {
              const hexKey = codePoints[0];
              const glyphData = inlineGlyphMap[hexKey];
              if (glyphData) {
                target[char] = glyphData;
                this.data[char] = glyphData;
                this.invalidateAdvance(char);
                loadingGlyphs.delete(char);
                return glyphData;
              }
            }

            // Mark as failed to avoid repeat work and skip network access in OBJKT mode
            loadingGlyphs.delete(char);
            failedGlyphs.add(char);
            return this.getEmojiFallback(char, target);
          }
          
          // Only make API calls when NOT in OBJKT mode
          if (!isObjktMode) {
            // Queue the glyph for batch fetching instead of individual requests
            queueGlyphFetch(char, codePointStr, target);
          } else {
            // In OBJKT mode for non-MatrixChunky8 fonts, create simple fallback
            const simpleFallback = {
              resolution: [6, 8],
              pixels: [
                [0, 1, 1, 1, 1, 0],
                [1, 0, 0, 0, 0, 1],
                [1, 0, 1, 1, 0, 1],
                [1, 0, 1, 1, 0, 1],
                [1, 0, 0, 0, 0, 1],
                [1, 0, 0, 0, 0, 1],
                [0, 1, 1, 1, 1, 0],
                [0, 0, 0, 0, 0, 0]
              ]
            };
            target[char] = simpleFallback;
            loadingGlyphs.delete(char);
          }

          // Return appropriate fallback immediately while loading
          return this.getEmojiFallback(char, target);
        },
      });
    }
    return this;
  }

  // Helper method to get appropriate fallback for different character types
  getEmojiFallback(char, target) {
    if (!char || char.length === 0) {
      return this.getLoadingPlaceholder();
    }

    // For MatrixChunky8, show loading placeholder
    if (this.name === "MatrixChunky8") {
      return this.getLoadingPlaceholder(4, 8); // MatrixChunky8 size
    }
    
    // For unifont, show loading placeholder
    if (this.name === "unifont" || this.data?.bdfFont === "unifont-16.0.03" || this.data?.bdfFont) {
      return this.getLoadingPlaceholder(8, 16); // Unifont size
    }

    const codePoint = char.codePointAt(0);

    // Check if it's an emoji (rough heuristic)
    if (codePoint >= 0x1f600 && codePoint <= 0x1f64f) {
      // Emoticons block - use simple smiley fallback
      return target["☺"] || target["?"] || null;
    } else if (codePoint >= 0x1f300 && codePoint <= 0x1f5ff) {
      // Miscellaneous Symbols and Pictographs - use question mark
      return target["?"] || null;
    } else if (codePoint >= 0x1f680 && codePoint <= 0x1f6ff) {
      // Transport and Map Symbols - use question mark
      return target["?"] || null;
    } else if (codePoint >= 0x2600 && codePoint <= 0x26ff) {
      // Miscellaneous Symbols - use question mark
      return target["?"] || null;
    } else {
      // For other missing characters, use standard fallback
      return target["?"] || null;
    }
  }
  
  // Create an animated loading placeholder for characters being fetched
  getLoadingPlaceholder(width = 8, height = 16) {
    // Use time to create animation effect
    const time = typeof performance !== 'undefined' ? performance.now() : Date.now();
    const frame = Math.floor(time / 100) % 4; // 4-frame animation at ~10fps
    
    // Create different patterns for each frame
    const pixels = [];
    for (let y = 0; y < height; y++) {
      const row = [];
      for (let x = 0; x < width; x++) {
        // Create a moving diagonal pattern
        const pattern = (x + y + frame) % 4;
        row.push(pattern < 2 ? 1 : 0);
      }
      pixels.push(row);
    }
    
    return {
      resolution: [width, height],
      pixels,
      advance: width,
      isPlaceholder: true, // Mark as placeholder so we know to repaint
    };
  }
  
  // Create a colored block placeholder for unifont characters that haven't loaded yet
  createColoredBlockPlaceholder() {
    // Generate random colors for the block (excluding pure black to ensure visibility)
    const colors = [];
    const numColors = Math.floor(Math.random() * 3) + 2; // 2-4 colors
    for (let i = 0; i < numColors; i++) {
      colors.push([
        Math.floor(Math.random() * 200) + 55, // R: 55-255
        Math.floor(Math.random() * 200) + 55, // G: 55-255
        Math.floor(Math.random() * 200) + 55, // B: 55-255
      ]);
    }
    
    return {
      resolution: [8, 16], // Standard unifont size
      pixels: [], // Empty pixels - will be drawn as box command
      commands: [
        // Fill the entire 8x16 block with random colored pixels
        ...Array.from({ length: 16 }, (_, y) =>
          Array.from({ length: 8 }, (_, x) => ({
            name: "point",
            args: [x, y],
            color: colors[Math.floor(Math.random() * colors.length)]
          }))
        ).flat()
      ],
      advance: 8, // Standard unifont advance width
    };
  }
  
  // Get a glyph for a specific character
  getGlyph(char) {
    try {
      return this.glyphs[char];
    } catch (err) {
      // Silently handle glyph access errors (e.g., fetch failures)
      console.warn(`Failed to get glyph for "${char}":`, err.message);
      return null;
    }
  }

  getAdvance(char) {
    if (!char) return this.blockWidth || 4;
    
    // For unifont, prefer actual BDF advance widths when glyphs are loaded
    // Fall back to 8px default only if glyph data isn't available yet
    if (this.name === "unifont" || this.data?.bdfFont === "unifont-16.0.03") {
      // Check if we have a loaded glyph with advance width
      try {
        const glyph = this.glyphs?.[char];
        if (glyph && typeof glyph.advance === "number") {
          return glyph.advance; // Use actual BDF DWIDTH value
        }
      } catch (err) {
        // Silently handle glyph access errors (e.g., fetch failures)
        // Will fall through to default 8px width
      }
      // Fall back to 8px (prevents layout jank during async glyph loading)
      return 8;
    }
    
    if (this.advanceCache.has(char)) {
      return this.advanceCache.get(char);
    }

    let advance;
    let glyph;

    // Check fonts.mjs advances FIRST (highest priority - allows per-font overrides)
    if (this.data?.advances && this.data.advances[char] !== undefined) {
      advance = this.data.advances[char];
    }

    // Safely access glyph (may trigger Proxy fetch which could fail)
    if (advance === undefined) {
      try {
        glyph = this.glyphs?.[char];
        if (glyph && typeof glyph.advance === "number") {
          advance = glyph.advance;
        }
      } catch (err) {
        // Silently handle glyph access errors
        // Will fall through to other fallback methods
      }
    }

    if (advance === undefined) {
      const glyphData = this.data?.[char];
      if (glyphData && typeof glyphData.advance === "number") {
        advance = glyphData.advance;
      }
    }

    if (advance === undefined && glyph?.dwidth?.x) {
      advance = glyph.dwidth.x;
    }

    if (advance === undefined && glyph?.resolution?.[0]) {
      advance = glyph.resolution[0];
    }

    if (advance === undefined && typeof this.data?.glyphWidth === "number") {
      advance = this.data.glyphWidth;
    }

    if (advance === undefined) {
      advance = this.blockWidth || 4;
    }

    this.advanceCache.set(char, advance);
    return advance;
  }

  invalidateAdvance(char) {
    if (!char) {
      this.advanceCache.clear?.();
    } else {
      this.advanceCache.delete?.(char);
    }
  }
  
  // 📓 tf.print
  print(
    $,
    pos = { x: undef, y: undef, size: 1, thickness: 1, rotation: 0 },
    lineNumber,
    text,
    bg = null,
    charColors = null,
  ) {
    // TODO: Pass printLine params through / make a state machine.
    const font = this.glyphs;
    const size = pos.size || 1;
    const blockMargin = 1;
    const inferredBlockWidth =
      this.data?.glyphWidth ?? this.blockWidth ?? this.data?.width ?? 6;
    const blockHeight = ((this.blockHeight || 10) + blockMargin) * size;
    const blockWidth = inferredBlockWidth;
    const thickness = pos.thickness || 1;
    const rotation = pos.rotation || 0;
    const fullWidth = blockWidth * size * text.length;

    if (Array.isArray(pos)) {
      pos = { x: pos[0], y: pos[1] };
    }

    const width = $.screen.width; // $.system?.world?.size
    // ? $.system.world.size.width
    // : $.screen.width;
    const height = $.screen.height; // $.system?.world?.size
    // ? $.system.world.size.height
    // : $.screen.height;

    // Calculate width properly for proportional fonts
    let w;
    const isProportional = this.data?.proportional === true || 
                          this.data?.bdfFont === "MatrixChunky8" ||
                          this.name === "MatrixChunky8";
    
    if (isProportional) {
      w = 0;
      const chars = [...text.toString()];
      for (const char of chars) {
        if (char === "\n") {
          continue;
        }
        const charAdvance = this.getAdvance(char) || inferredBlockWidth;
        w += charAdvance * size;
      }
    } else {
      // For monospace fonts, use the original calculation
      w = text.length * blockWidth * size;
    }

    // Randomize pos.x and pos.y if undefined.
    if (pos.center === undefined) {
      if (pos.right !== undefined) {
        pos.x = width - w - pos.right;
      } else if (pos.left !== undefined) {
        pos.x = pos.left;
      } else if (pos.x === undefined) {
        pos.x = $.num.randIntRange(-fullWidth / 2, width + fullWidth / 2);
      }

      if (pos.bottom !== undefined) {
        pos.y = height - blockHeight - pos.bottom;
      } else if (pos.top !== undefined) {
        pos.y = pos.top;
      } else if (pos.y === undefined) {
        pos.y = $.num.randIntRange(-blockHeight / 2, height + blockHeight / 2);
      }
    }

    // Set x, y position and override if centering is specified.
    // Floor coordinates to ensure pixel-aligned rendering, especially important for rotation
    let x = Math.floor(pos.x || 0),
      y = Math.floor(pos.y || 0);

    pos.center = pos.center || "";

    if (pos.center.includes("x")) {
      const hw = w / 2;
      x = pos.x === undef ? width / 2 - hw : x - hw;
    }

    if (pos.center.includes("y")) {
      const hh = Math.floor(blockHeight / 2);
      y = pos.y === undef ? height / 2 - hh : y - hh;
    }

    y += lineNumber * blockHeight;

    // Only print the line if it will be visible on screen now...
    // Deprecated because it's incompatible with pan.
    // TODO: Eventually this could be added but it
    //       needs to take into account the current panTranslation in graph.
    // 📔 Or culling should happen further down the line?

    // if (
    //   y < -blockHeight * size ||
    //   y > $.screen.height ||
    //   x > $.screen.width ||
    //   x < -w
    // ) {
    //   // Offscreen render.
    //   return;    // }

    const rn = $.inkrn(); // Remember the current ink color.

    // Background
    if (bg !== null && bg !== false) $.ink(bg).box(x, y, fullWidth, blockHeight);

    // if (text === "POW") console.log("POW PRINT 😫", x, y, width, height);    // Check if we have per-character colors
    // Apply baseline adjustment for fonts that need bottom alignment
    const baselineAdjustment = this.data.baseline || 0;
    y += baselineAdjustment;

    // Convert rotation to radians for calculations
    const rotRad = (rotation * Math.PI) / 180;
    const cosR = Math.cos(rotRad);
    const sinR = Math.sin(rotRad);

    if (charColors && charColors.length > 0) {
      // Render each character with its own color
      let currentX = 0; // Start at 0, we'll rotate relative to origin
      for (let i = 0; i < text.length; i++) {
        const char = text[i];
        const charColor = charColors[i];

        // Set color for this character
        if (charColor) {
          // Handle background color syntax (object with foreground and background)
          if (typeof charColor === 'object' && charColor.foreground !== undefined && charColor.background !== undefined) {
            // Draw background first
            if (charColor.background) {
              const charAdvance = this.getAdvance(char);
              const charWidth = charAdvance * size;
              if (Array.isArray(charColor.background)) {
                $.ink(...charColor.background);
              } else {
                $.ink(charColor.background);
              }
              // Rotate background position
              const rotatedX = x + (currentX * cosR - 0 * sinR);
              const rotatedY = y + (currentX * sinR + 0 * cosR);
              $.box(rotatedX, rotatedY, charWidth, blockHeight);
            }
            
            // Set foreground color
            if (charColor.foreground) {
              if (Array.isArray(charColor.foreground)) {
                $.ink(...charColor.foreground);
              } else {
                $.ink(charColor.foreground);
              }
            } else {
              $.ink(...rn); // Use original color if no foreground specified
            }
          } else if (Array.isArray(charColor)) {
            $.ink(...charColor);
          } else {
            $.ink(charColor);
          }
        } else {
          $.ink(...rn); // Use original color if no specific color
        }

        // Calculate rotated position for this character
        const rotatedX = Math.round(x + (currentX * cosR - 0 * sinR));
        const rotatedY = Math.round(y + (currentX * sinR + 0 * cosR));

        // Render single character with rotation
        $.printLine(
          char,
          font,
          rotatedX,
          rotatedY,
          blockWidth,
          size,
          0,
          thickness,
          rotation,
          this.data, // Pass font metadata to avoid BDF proxy issues
        );

        // Move to next character position using proper advance width
        const charAdvance = this.getAdvance(char);
        currentX += charAdvance * size;
      }
    } else {
      // Original single-color rendering with rotation applied to each character position
      if (rotation !== 0) {
        // For rotated text, render character by character with position transforms
        let currentX = 0;
        for (let i = 0; i < text.length; i++) {
          const char = text[i];
          
          // Calculate rotated position for this character
          const rotatedX = Math.round(x + (currentX * cosR - 0 * sinR));
          const rotatedY = Math.round(y + (currentX * sinR + 0 * cosR));
          
          $.ink(...rn).printLine(
            char,
            font,
            rotatedX,
            rotatedY,
            blockWidth,
            size,
            0,
            thickness,
            rotation,
            this.data,
          );
          
          // Move to next character position
          const charAdvance = this.getAdvance(char);
          currentX += charAdvance * size;
        }
      } else {
        // No rotation - use original fast path
        $.ink(...rn).printLine(
          text,
          font,
          x,
          y,
          blockWidth,
          size,
          0,
          thickness,
          rotation,
          this.data, // Pass font metadata to avoid BDF proxy issues
        ); // Text
      }
    }
  }
}

// An interactive text prompt object.
class TextInput {
  $; // a reference to the api.
  #text; // text content
  #lastPrintedText = ""; // a place to cache a previous reply.
  #lastUserText = ""; // cache the user's in-progress edited text.
  submittedText = ""; // cache the user's submitted text.

  mute = false; // Whether to prevent sounds from playing.

  shifting = false; // Whether we are emoving the cursor or not.

  #renderSpaces = false; // Whether to render invisible space characters. " "
  //                        For debugging purposes.

  blink; // block cursor blink timer
  showBlink = true;
  cursor = "blink";

  // Buttons
  enter; // A button for replying or inputting text.
  copy; // A button for copying to the clipboard, that shows up conditionally.
  paste; // Similar to copy.

  canType = false;

  //#autolock = true;
  #lock = false;
  #lockTimeout;
  #showSpinner = false;

  #prompt;

  hideGutter = false;
  #gutterMax;
  #activatingPress = false;
  #edgeCancelled = false;
  #manuallyDeactivated = false;
  #manualDeactivationTime = 0;
  #manuallyActivated = false;
  #manualActivationTime = 0;
  #preventDeactivation = false; // Flag to prevent unwanted deactivation during command execution


  typeface;
  pal; // color palette
  scheme;

  #processCommand; // text processing callback
  // #processingCommand = false;
  historyDepth = -1;
  #prehistory;

  //inputStarted = false; // Flipped when the TextInput is first activated.
  //                       (To clear any starting text.)
  #moveThreshold = 6; // Drag threshold.
  #moveDeltaX = 0;
  #recentlyShifting = false; // Track if we just finished character sliding

  runnable = false; // Whether a command can be tried.
  didReset; // Callback for blank reset.

  key;

  copiedCallback; // When the "Copy" button is pressed, for designing wrappers.

  #copyPasteTimeout; // UI Timer for clipboard copy response.
  #copyPasteScheme; // An override for the Copy button's color.

  #coatedCopy; // Stores a version of the current text output that could be
  //              decorated. (With a URL, for example.)

  activate; // Hook to `activate` inside of act.
  activated; // Optional callback for when the the text input becomes
  //            activated via pushing the Enter button or typing a key.
  activatedOnce = false;
  backdropTouchOff = false; // Determines whether to activate the input
  //                           after tapping the backdrop.
  commandSentOnce = false; // 🏴

  closeOnEmptyEnter = false;

  // Add support for loading from preloaded system typeface.
  constructor(
    $,
    text = "",
    processCommand,
    options = {
      palette: undefined,
      font: "font_1" /*"unifont"*/, // fonts.font_1,
      //autolock: true,
      wrap: "char",
    },
  ) {
    this.key = `${$.slug}:history`; // This is "per-piece" and should
    //                                 be per TextInput object...23.05.23.12.50

    this.$ = $;

    this.closeOnEmptyEnter = options.closeOnEmptyEnter || false;
    this.hideGutter = options.hideGutter || false;
    // ^ Close keyboard on empty entry.

    this.copiedCallback = options.copied; // Load typeface, preventing double loading of the system default.
    if (!options.font) options.font = "font_1"; // Use preloaded font as needed.

    // Flag to track if we need to repaint due to async glyph loading
    this._needsRepaint = false;

    if ($.typeface?.data !== options.font) {
      this.typeface = new Typeface(options.font); // Load custom typeface.
      this.#moveThreshold = this.typeface.blockWidth;
      // Pass needsPaint callback for async glyph loading
      this.typeface.load($.net.preload, () => {
        this._needsRepaint = true;
      });
    } else {
      this.typeface = $.typeface; // Set to system typeface.
    }

    this.activated = options.activated;
    //this.#autolock = options.autolock;
    this.didReset = options.didReset;

    const blockWidth = this.typeface.blockWidth;
    this.#gutterMax = options.gutterMax || 48;

    this.#prompt = new Prompt(
      6, // blockWidth,
      6, // blockWidth,
      options.wrap || "char", // "char" or "word"
      $.store["gutter:lock"] ||
        Math.min(this.#gutterMax, floor($.screen.width / blockWidth) - 2),
      options.lineSpacing,
      this.typeface,
    );

    this.print(text); // Set initial text.

    this.startingInput = this.text;
    this.scheme = options.scheme || {
      dark: {
        text: 255,
        background: 0,
        block: 255,
        highlight: 0,
        guideline: 255,
      },
      light: {
        text: 0,
        background: 255,
        block: 0,
        highlight: 255,
        guideline: 0,
      },
    };

    const {
      ui: { TextButton: TB },
    } = $;

    this.enter = new TB(this.scheme.buttons?.enter || "Enter");
    this.enter.stickyScrubbing = true; // Prevent drag-between-button behavior
    this.enter.btn.stickyScrubbing = true; // Also set on the actual button object
    this.enter.btn.noEdgeDetection = true; // Prevent cancellation from edge detection
    this.copy = new TB(this.scheme.buttons?.copy.label || "Copy");
    this.paste = new TB(this.scheme.buttons?.paste?.label || "Paste");
    this.copy.btn.disabled = true; // Copy is disabled by default,
    this.paste.btn.disabled = true; // as is Paste.

    if (this.text.length === 0) {
      this.enter.btn.disabled = true;
    }

    this.#processCommand = processCommand;
    $.send({ type: "keyboard:enabled" });
  }

  // 🔤 Change the font dynamically
  async setFont(fontName) {
    if (!fontName || fontName === this.typeface?.name) return;
    
    // Load the new typeface
    const newTypeface = new Typeface(fontName);
    await newTypeface.load(this.$.net.preload, () => {
      this._needsRepaint = true;
    });
    
    this.typeface = newTypeface;
    this.#moveThreshold = newTypeface.blockWidth;
    
    // Update the prompt with the new typeface
    const blockWidth = newTypeface.blockWidth;
    this.#prompt.typeface = newTypeface;
    this.#prompt.letterWidth = blockWidth;
    this.#prompt.letterHeight = newTypeface.blockHeight;
    
    this._needsRepaint = true;
  }

  // Stretches the gutter to be the screen width minus two slots.
  fullGutter($) {
    this.gutter = Math.min(
      this.#gutterMax,
      floor($.screen.width / this.#prompt.letterWidth) - 2,
    );
  }

  set lock(bool) {
    this.#lock = bool;
    if (bool) {
      this.#lockTimeout = setTimeout(() => {
        this.#showSpinner = true;
      }, 100);
    } else {
      clearTimeout(this.#lockTimeout);
      this.#showSpinner = false;
    }
  }

  get lock() {
    return this.#lock;
  }

  get recentlyShifting() {
    return this.#recentlyShifting;
  }

  // Adjust the gutter width for text wrapping.
  set gutter(n) {
    this.#prompt.colWidth = n;
    this.#prompt.gutter = this.#prompt.colWidth * this.#prompt.letterWidth;
  }

  // Alias for the setter above, returned in columns.
  get columns() {
    return this.#prompt.colWidth;
  }

  // Reset the user text after a message is complete.
  clearUserText() {
    this.#lastUserText = "";
  }

  addUserText(txt) {
    this.#lastUserText = txt;
  }

  // Snap cursor to the end of text.
  snap() {
    this.#prompt.snapTo(this.text);
    this.$.send({
      type: "keyboard:cursor",
      content: { cursor: this.#text.length },
    });
  }

  // Run a command
  async run(store) {
    this.snap();
    this.submittedText = "";
    await this.#execute(store); // Send a command.
  }

  // Set the text and reflow it.
  set text(str) {
    if (str === this.#text) {
      // console.warn("Redundant text");
      return;
    }
    this.#text = str;
    this.flow();
    // console.log("📝 Setting text to:", str);
  }

  // Return the prompt.
  get prompt() {
    return this.#prompt;
  }

  print(text) {
    this.text = text;
    this.bakePrintedText();
  }

  bakePrintedText() {
    this.#lastPrintedText = this.text;
  }

  latentFirstPrint(text) {
    if (!this.activatedOnce) this.text = text;
  }

  // Reflow the input text.
  flow() {
    this.#prompt.mapTo(this.text); // Rebuild the text map index.
  }

  // Return the text contents of the input.
  get text() {
    return this.#text;
  }

  #coatCopy(text) {
    return this.copiedCallback?.(text) || text;
  }

  // Paint the TextInput, with an optional `frame` for placement.
  paint(
    $,
    clear = false,
    frame = { x: 0, y: 0, width: $.screen.width, height: $.screen.height },
  ) {
    this.pal =
      this.scheme[$.dark ? "dark" : "light"] ||
      this.scheme["dark"] ||
      this.scheme;

    // Check if mouse is hovering over the prompt frame
    const pen = $.pen;
    const isHovering = pen && 
      pen.x >= frame.x && 
      pen.x < frame.x + frame.width && 
      pen.y >= frame.y && 
      pen.y < frame.y + frame.height;

    if (!clear && this.pal.background !== undefined) {
      $.ink(this.pal.background).box(frame); // Paint bg.
      
      // Add subtle hover glow effect on the prompt area
      if (isHovering && this.canType) {
        const glowColor = this.pal.promptHover || 
          ($.dark ? [100, 80, 150, 30] : [180, 150, 220, 40]);
        $.ink(glowColor).box(frame.x, frame.y, frame.width, frame.height);
        
        // Draw a subtle inner border highlight
        const borderColor = this.pal.promptHoverBorder ||
          ($.dark ? [150, 100, 200, 80] : [140, 100, 180, 60]);
        $.ink(borderColor).box(frame.x, frame.y, frame.width, frame.height, "inline");
      }
    }
    const ti = this;
    const prompt = this.#prompt;

    // Build character-to-color map for KidLisp syntax highlighting
    let charColorMap = null;
    if ($.system?.prompt?.kidlispMode && this.text) {
      try {
        const tokens = tokenize(this.text);
        const kidlispInstance = new KidLisp();
        kidlispInstance.isEditMode = true; // Enable edit mode to prevent transparent text
        kidlispInstance.initializeSyntaxHighlighting(this.text);
        
        charColorMap = new Map();
        let sourceIndex = 0;
        
        // Process each token
        for (let i = 0; i < tokens.length; i++) {
          const token = tokens[i];
          
          // Check if this token is a fastmath expression like "frame*3"
          const fastMathMatch = token.match(/^(\w+)\s*([+\-*/%])\s*(\w+|\d+(?:\.\d+)?)$/);
          if (fastMathMatch) {
            const [, left, op, right] = fastMathMatch;
            
            // Find the full token in the source
            const tokenIndex = this.text.indexOf(token, sourceIndex);
            if (tokenIndex !== -1) {
              // Color each part of the fastmath expression
              const leftColor = kidlispInstance.getTokenColor(left, [left, op, right], 0);
              const opColor = kidlispInstance.getTokenColor(op, [left, op, right], 1);
              const rightColor = kidlispInstance.getTokenColor(right, [left, op, right], 2);
              
              // Map characters for left part
              for (let j = 0; j < left.length; j++) {
                charColorMap.set(tokenIndex + j, leftColor);
              }
              // Map operator
              charColorMap.set(tokenIndex + left.length, opColor);
              // Map characters for right part
              for (let j = 0; j < right.length; j++) {
                charColorMap.set(tokenIndex + left.length + 1 + j, rightColor);
              }
              
              sourceIndex = tokenIndex + token.length;
            }
            continue; // Skip normal token processing
          }
          
          // Check if this is a fade expression that needs special coloring
          if (token.startsWith("fade:")) {
            const colorName = kidlispInstance.getTokenColor(token, tokens, i);
            
            // Use colorFadeExpression to get the properly colored string
            const coloredFadeString = kidlispInstance.colorFadeExpression(token);
            
            // Parse the colored string to extract color assignments
            // Format: \color1\text1\color2\text2...
            const colorCodeRegex = /\\([^\\]+)\\([^\\]*)/g;
            let match;
            let charOffset = 0;
            const tokenIndex = this.text.indexOf(token, sourceIndex);
            
            while ((match = colorCodeRegex.exec(coloredFadeString)) !== null) {
              const color = match[1];
              const text = match[2];
              
              // Map each character in this text segment to its color
              for (let j = 0; j < text.length; j++) {
                if (tokenIndex >= 0 && charOffset < token.length) {
                  charColorMap.set(tokenIndex + charOffset, color);
                  charOffset++;
                }
              }
            }
            
            if (tokenIndex !== -1) {
              sourceIndex = tokenIndex + token.length;
            }
          } else {
            // Normal token - get single color
            const colorName = kidlispInstance.getTokenColor(token, tokens, i);
            
            // Find token position in source
            const tokenIndex = this.text.indexOf(token, sourceIndex);
            if (tokenIndex !== -1) {
              // Map each character in the token to its color
              for (let charOffset = 0; charOffset < token.length; charOffset++) {
                charColorMap.set(tokenIndex + charOffset, colorName);
              }
              sourceIndex = tokenIndex + token.length;
            }
          }
        }
      } catch (error) {
        console.warn("Error building KidLisp syntax highlight map:", error);
        charColorMap = null;
      }
    }

    function paintBlockLetter(char, pos, alt = false, charIndex = -1) {
      if (char.charCodeAt(0) === 10 && ti.#renderSpaces) {
        $.ink([255, 0, 0, 127]).box(pos.x, pos.y, 4);
      } else if (char !== " " && char.charCodeAt(0) !== 10) {
        //

        const pic = ti.typeface.glyphs[char] || ti.typeface.glyphs["?"];

        // Use syntax highlighting color if in KidLisp mode and we have a color for this character
        let drawColor;
        if (charColorMap && charIndex >= 0 && charColorMap.has(charIndex)) {
          const colorName = charColorMap.get(charIndex);
          
          // Check if it's an RGB string like "255,0,0" or "255,255,255,0"
          if (colorName && colorName.includes(",")) {
            const parts = colorName.split(",").map(n => parseInt(n.trim()));
            drawColor = parts.length >= 3 ? parts : null;
          }
          // Check for compound colors (like $codes)
          else if (colorName && colorName.startsWith("COMPOUND:")) {
            // For now, just use the first color part
            const parts = colorName.split(":");
            const actualColorName = parts[1] || "white";
            if (cssColors[actualColorName]) {
              drawColor = cssColors[actualColorName];
            } else {
              drawColor = [255, 255, 255];
            }
          }
          // Handle special markers (should not appear in prompt, but just in case)
          else if (colorName === "RAINBOW" || colorName === "ZEBRA") {
            drawColor = !alt ? ti.pal.text : ti.pal.prompt || ti.pal.text;
          }
          // Convert color name to RGB
          else if (cssColors[colorName]) {
            drawColor = cssColors[colorName];
          } else if (colorName === "gray" || colorName === "grey") {
            drawColor = [128, 128, 128];
          } else if (colorName === "orange") {
            drawColor = [255, 165, 0];
          } else if (colorName === "lime") {
            drawColor = [0, 255, 0];
          } else if (colorName === "pink") {
            drawColor = [255, 192, 203];
          } else if (colorName === "darkred") {
            drawColor = [139, 0, 0];
          } else if (colorName === "limegreen") {
            drawColor = [50, 205, 50];
          } else if (colorName === "hotpink") {
            drawColor = [255, 105, 180];
          } else if (colorName === "mediumseagreen") {
            drawColor = [60, 179, 113];
          } else {
            // Fallback to default color
            drawColor = !alt ? ti.pal.text : ti.pal.prompt || ti.pal.text;
          }
          
          // If we still don't have a valid color, use default
          if (!drawColor) {
            drawColor = !alt ? ti.pal.text : ti.pal.prompt || ti.pal.text;
          }
        } else {
          drawColor = !alt ? ti.pal.text : ti.pal.prompt || ti.pal.text;
        }

        // 🌞 LIGHT MODE SHADOWS: For bright colors on light background, draw shadow first
        const isLightMode = !$.dark;
        if (isLightMode && charColorMap && charIndex >= 0 && charColorMap.has(charIndex)) {
          // Calculate luminance to check if color needs shadow
          const [r, g, b] = Array.isArray(drawColor) ? drawColor : [200, 200, 200];
          const luminance = (0.299 * r + 0.587 * g + 0.114 * b);
          
          if (luminance > 120) {
            // Draw dark purple-blue shadow offset by 1 pixel
            $.ink([30, 20, 50, 180]).draw(
              pic,
              { x: pos.x + 1, y: pos.y + 1 },
              prompt.scale,
            );
          }
        }

        $.ink(drawColor).draw(
          pic,
          pos,
          prompt.scale,
        ); // Draw each character.
      } else if (ti.#renderSpaces) {
        $.ink([0, 255, 0, 127]).box(pos.x, pos.y, 3);
      }
    }

    // 🗺️ Render the text from the maps! (Can go both ways...)
    if (frame.x || frame.y) $.pan(frame.x, frame.y);

    if (!this.#lock && this.selection && this.canType) {
      for (let i = this.selection[0]; i < this.selection[1]; i += 1) {
        const c = prompt.textToCursorMap[i];
        const p = prompt.pos(c, true);
        $.ink(this.pal.selection || [255, 255, 0, 64]).box(p);
      }
    }

    // A. Draw all text from displayToTextMap.
    let submittedIndex = 0;
    Object.keys(prompt.cursorToTextMap).forEach((key) => {
      const [x, y] = key.split(":").map((c) => parseInt(c));
      const charIndex = prompt.cursorToTextMap[key];
      const char = this.text[charIndex];
      let fromSubmitted = false;
      if (!this.canType && submittedIndex < this.submittedText.length) {
        if (char === this.submittedText[submittedIndex]) fromSubmitted = true;
        submittedIndex += 1;
      }

      // console.log(this.submittedText, this.text)
      // if (this.canType || !this.commandSentOnce || (this.lock && this.text === this.submittedText))
      //   fromSubmitted = true;
      paintBlockLetter(char, prompt.pos({ x, y }), fromSubmitted, charIndex);
    });

    // Or...
    // B. Draw all text from textToDisplayMap
    //    TODO: Include the submitted text index. 23.07.28.15.47
    // prompt.textToCursorMap.forEach((pos, i) => {
    //   const char = this.text[i];
    //   paintBlockLetter(char, prompt.pos(pos));
    // });

    if (this.canType) {
      if (!this.hideGutter) {
        $.ink(this.pal.guideline).line(
          prompt.gutter,
          0,
          prompt.gutter,
          frame.height - 1,
        ); // Ruler
      }
      $.ink($.dark ? 127 : "teal").box(
        0,
        0,
        frame.width,
        frame.height,
        "inline",
      ); // Focus
    }

    if (this.#lock) {
      if (this.#showSpinner) {
        // Show a spinner if the prompt is "locked".
        const center = $.geo.Box.from(prompt.pos()).center;
        const distance = 2; // You can adjust this value as per your needs

        const topL = [center.x - distance, center.y - distance];
        const topR = [center.x + distance, center.y - distance];
        const bottomL = [center.x - distance, center.y + distance];
        const bottomR = [center.x + distance, center.y + distance];
        const middleL = [center.x - distance, center.y];
        const middleR = [center.x + distance, center.y];

        $.ink(this.pal.block);
        if ($.paintCount % 60 < 20) {
          $.line(...topR, ...bottomL);
        } else if ($.paintCount % 60 < 40) {
          $.line(...middleL, ...middleR);
        } else {
          $.line(...topL, ...bottomR);
        }
      }
    } else {
      if (this.cursor === "blink" && this.showBlink && this.canType) {
        // Use green cursor only for actual KidLisp code, not for nopaint brushes
        const cursorColor = $.system?.prompt?.actualKidlisp
          ? $.dark
            ? [100, 255, 100]
            : [0, 150, 0]
          : this.pal.block;
        $.ink(cursorColor).box(prompt.pos(undefined, true)); // Draw blinking cursor.
        const char = this.text[this.#prompt.textPos()];
        // Only draw inverted character if there's actually a character there
        if (char !== undefined && char !== "") {
          const pic = this.typeface.glyphs[char];
          if (pic)
            $.ink(this.pal.highlight).draw(pic, prompt.pos(undefined, true));
        }
      }
    }

    if (this.cursor === "stop" && !this.canType) {
      const pos = prompt.pos();
      $.ink(255, 0, 0).box(pos.x + 1, pos.y + 3, 3);
    }

    // Build custom color schemes for the `ui.TextButton`s if they were defined.
    let btnScheme, btnHvrScheme, btnRolloverScheme;
    const pal = this.pal;
    if (pal.btn && pal.btnTxt)
      btnScheme = [pal.btn, pal.btnTxt, pal.btnTxt, pal.btn];
    if (pal.btnHvr && pal.btnHvrTxt)
      btnHvrScheme = [pal.btnHvr, pal.btnHvrTxt, pal.btnHvrTxt, pal.btnHvr];
    if (pal.btnRollover && pal.btnRolloverTxt)
      btnRolloverScheme = [pal.btnRollover, pal.btnRolloverTxt, pal.btnRolloverTxt, pal.btnRollover];

    // Enter Button
    if (!this.enter.btn.disabled) {
      // Outline the whole screen.
      if (this.#activatingPress) {
        const color =
          pal.focusOutline ||
          (Array.isArray(pal.text)
            ? [...pal.text.slice(0, 3), 128]
            : [255, 0, 200, 64]);

        // Highlight outline.
        $.ink(color).box(0, 0, frame.width, frame.height, "inline");
      }
    }

    if (frame.x || frame.y) $.unpan();

    if (!this.enter.btn.disabled) {
      this.enter.reposition({ right: 6, bottom: 6, screen: frame });
      $.layer(2);
      this.enter.paint($, btnScheme, btnHvrScheme, undefined, btnRolloverScheme);
      $.layer(1);
    }

    // Copy Button
    if (!this.copy.btn.disabled) {
      this.copy.reposition({ left: 6, bottom: 6, screen: frame });
      this.copy.btn.publishToDom($, "copy", this.#coatedCopy);
      $.layer(2);
      this.copy.paint(
        { ink: $.ink },
        this.#copyPasteScheme || btnScheme,
        btnHvrScheme,
        undefined,
        btnRolloverScheme,
      );
      $.layer(1);
    }

    // Paste Button
    if (!this.paste.btn.disabled) {
      this.paste.reposition({ left: 6, bottom: 6, screen: frame });
      this.paste.btn.publishToDom($, "paste");
      $.layer(2);
      this.paste.paint(
        { ink: $.ink },
        this.#copyPasteScheme || btnScheme,
        btnHvrScheme,
        undefined,
        btnRolloverScheme,
      );
      $.layer(1);
    }

    // Return false if we have loaded every glyph.
    // (Can be wired up to the return value of the parent's `paint`)
    return !(
      keys(this.typeface.glyphs).length === keys(this.typeface.glyphData).length
    );
  }
  // Simulate anything necessary.
  sim({ seconds, needsPaint, gizmo: { Hourglass } }) {
    this.blink =
      this.blink ||
      new Hourglass(seconds(0.75), {
        flipped: (count, showBlinkOverride) => {
          if (showBlinkOverride !== undefined)
            this.showBlink = showBlinkOverride;
          else this.showBlink = !this.showBlink;
          needsPaint();
        },
        autoFlip: true,
      });

    if (this.#lock) needsPaint();
    if (this.canType) this.blink.step();

    // Check if we need to repaint due to async glyph loading
    if (this._needsRepaint) {
      this._needsRepaint = false;
      needsPaint();
    }
  }

  // Helper method to ensure blink is initialized before use
  #ensureBlink() {
    if (!this.blink) {
      // Initialize blink with a basic Hourglass if sim hasn't run yet
      const Hourglass = this.$.gizmo?.Hourglass;
      if (Hourglass) {
        this.blink = new Hourglass(45, { // 0.75 seconds at 60fps
          flipped: (count, showBlinkOverride) => {
            if (showBlinkOverride !== undefined)
              this.showBlink = showBlinkOverride;
            else this.showBlink = !this.showBlink;
            this.$.needsPaint?.();
          },
          autoFlip: true,
        });
      }
    }
  }

  showButton($, { nocopy, nopaste } = { nocopy: false, nopaste: false }) {
    this.enter.btn.disabled = false;
    if (!nocopy && this.text.length > 0) {
      this.#coatedCopy = this.#coatCopy(this.text); // Wrap text to be copied.
      this.copy.btn.disabled = false;
      this.paste.btn.disabled = true; // Disable paste button.
      this.paste.btn.removeFromDom($, "paste");
    } else if (nopaste) {
      // Force turning off the paste button.
      this.paste.btn.disabled = true; // Disable paste button.
      this.paste.btn.removeFromDom($, "paste");
    } else {
      this.paste.btn.disabled = false; // Enable paste button.
    }
  }

  // Run a command.
  async #execute(store) {
    // Make a history stack if one doesn't exist already.
    store[this.key] = store[this.key] || [];
    // Push input to a history stack, avoiding repeats and prompt-prefixed navigation.
    if (store[this.key][0] !== this.text && !this.text.startsWith("prompt~")) {
      store[this.key].unshift(this.text);
    }
    // console.log("📚 Stored prompt history:", store[key]);
    store.persist(this.key); // Persist the history stack across tabs.
    // 🍎 Process commands for a given context, passing the text input.
    await this.#processCommand?.(this.text);
    this.commandSentOnce = true;
  }

  // Clear the TextInput object and flip the cursor to ON.
  blank(cursor) {
    if (cursor) this.cursor = cursor;
    this.text = "";
    this.#prompt.cursor = { x: 0, y: 0 };
    this.blink?.flip(true);
    this.$.send({ type: "keyboard:text:replace", content: { text: "" } });
  }

  // Set the UI state to be that of a completed reply.
  replied($) {
    this.runnable = false;
    //this.inputStarted = false;
    this.canType = false;
    this.clearUserText();
    this.showButton($);
  }

  #buildCopyPasteScheme() {
    // Use default or set custom scheme for inactive button reply.
    let scheme = [64, 127, 127, 64];
    if (this.pal.btnReply && this.pal.btnReplyTxt)
      scheme = [
        this.pal.btnReply,
        this.pal.btnReplyTxt,
        this.pal.btnReplyTxt,
        this.pal.btnReply,
      ];
    return scheme;
  }

  // Handle user input.
  async act($) {
    const { debug, event: e, store, needsPaint, sound } = $;

    // Handle UI interaction cancellation when pointer leaves window
    if (e.is("ui:cancel-interactions")) {
      // Mark that edge cancellation happened and cancel the activation state
      this.#edgeCancelled = true;
      this.#activatingPress = false; // Cancel the activation state immediately

      // Reset any backdrop touch state to ensure fresh interactions
      this.backdropTouchOff = false;

      // Handle all buttons to ensure proper cancellation
      this.enter.btn.act(e);
      this.copy.btn.act(e);
      this.paste.btn.act(e);
      needsPaint(); // Repaint to remove the activation outline
      return;
    }

    // Reflow the prompt on frame resize.
    if (e.is("reframed")) {
      if (!$.store["gutter:lock"]) this.fullGutter($);
      this.flow();
      needsPaint();
    }

    // ⌨️ Add text via the keyboard.
    if (e.is("keyboard:down") && this.#lock === false && !this.enter.btn.down) {
      // Reset edge cancellation when user actively starts typing
      if (e.key.length === 1 && e.ctrl === false && e.key !== "`") {
        this.#edgeCancelled = false;
      }
      
      // 🔡 Inserting an individual character.
      if (e.key.length === 1 && e.ctrl === false && e.key !== "`") {
        // Auto-activate TextInput when user starts typing if not already active
        if (!this.canType && !this.#edgeCancelled) {
          this.#manuallyActivated = true;
          this.#manualActivationTime = Date.now();
          activate(this);
        }
        // if (this.text === "" && e.key === " ") {
        //   this.blink.flip(true);
        //   return; // Skip opening spaces.
        // }

        // Printable keys with subbed punctuation.
        let insert = e.key.replace(/[“”]/g, '"').replace(/[‘’]/g, "'");
        let index = this.#prompt.textPos();

        const char = this.text[index];
        const newLine = char?.charCodeAt(0) === 10;

        const underCursor = index !== undefined;

        // If the cursor is in the corner but comes before any text.
        if (
          index === undefined &&
          this.#prompt.cursor.x === 0 &&
          this.#prompt.cursor.y === 0
        ) {
          index = 0;
          this.text = insert + this.text;
          this.#prompt.forward(this.#prompt.cursor, insert.length);
          this.#ensureBlink();
          this.blink?.flip(true);
          this.showBlink = true;
          return;
        }

        if (newLine && underCursor) {
          // Double up the characters so that this new character exists
          // at the position of the current cursor.
          this.text =
            this.text.slice(0, index + 1) + insert + this.text.slice(index + 1);
          this.#prompt.forward();
          this.#ensureBlink();
          this.blink?.flip(true);
          this.showBlink = true;
          return;
        }

        // Move backwards until we reach a character
        while (index === undefined) {
          index = this.#prompt.textPos(
            this.#prompt.backward({ ...this.#prompt.cursor }),
          );
        }

        const sliceIndex = underCursor ? index : index + 1;

        // If we are on the first char of a wrapped word, and inserting spaces,
        // then add enough spaces through to the end of the line.
        if (this.#prompt.wrapped(sliceIndex) && insert === " ") {
          const lastCursor = this.#prompt.textToCursorMap[sliceIndex - 1];
          const thisCursor = this.#prompt.cursor;

          let spaces = 0;
          spaces = this.#prompt.colWidth - lastCursor.x + thisCursor.x;
          insert = " ".repeat(spaces - 1);
        }

        this.text =
          this.text.slice(0, sliceIndex) + insert + this.text.slice(sliceIndex);

        if (!underCursor || index === 0) {
          // Append at end of line.
          let skipForward = false;

          const newIndex = this.#prompt.textPos();
          const mapped = this.#prompt.textToCursorMap[newIndex];
          if (mapped) {
            this.#prompt.cursor = { ...mapped };
          } else {
            skipForward = true;
          }
          if (newIndex <= index && index > 0) {
            // We broke a line so jump ahead the difference.
            this.#prompt.forward(this.#prompt.cursor, index - newIndex + 2);
          } else if (!skipForward) this.#prompt.forward(); // Move forward a space.
        } else {
          let newCursor =
            this.#prompt.textToCursorMap[sliceIndex + insert.length];
          // Check for the skipped new line character.
          if (!newCursor)
            newCursor =
              this.#prompt.textToCursorMap[sliceIndex + insert.length + 1];
          if (newCursor) this.#prompt.cursor = { ...newCursor };
        }
      } else {
        // Other keys.
        if (e.key === "Backspace") {
          const prompt = this.#prompt;

          // Detect if the cursor is on a `\n` new line character.
          // It could potentially be on "two characters"...
          const currentTextIndex = prompt.textPos();
          const onNewline = this.text[currentTextIndex]?.charCodeAt(0) === 10;

          if (onNewline) {
            this.text =
              this.text.slice(0, currentTextIndex) +
              this.text.slice(currentTextIndex + 1);
            if (this.text.length === currentTextIndex) {
              this.snap();
            } else {
              prompt.crawlBackward();
              if (prompt.posHasVisibleCharacter()) prompt.forward();
            }
          } else {
            // Otherwise continue with a normal backspace action.

            // Move an invisible cursor back and retrieve the text index for it.
            const back = prompt.backward({ ...prompt.cursor });
            const key = `${back.x}:${back.y}`;

            const cursorTextIndex = prompt.cursorToTextMap[key];
            const hasNewLine =
              prompt.cursorToTextMap[key + ":\\n"] !== undefined;

            const currentPosition = prompt.textPos();
            if (prompt.wrapped(currentPosition)) {
              // Delete backwards from text position through any
              // spaces until the last visible character.

              let movablePosition = prompt.textPos();
              let char = this.text[movablePosition - 1];
              let len = 0;
              while (char === " ") {
                movablePosition -= 1;
                char = this.text[movablePosition - 1];
                len += 1;
              }

              // Remove `len` characters from the text.
              if (len > 0) {
                this.text =
                  this.text.slice(0, currentPosition - len) +
                  this.text.slice(currentPosition);
                prompt.cursor = {
                  ...prompt.textToCursorMap[currentPosition - len],
                };
              }

              this.#ensureBlink();
              this.blink?.flip(true);
              this.showBlink = true;
              return;
            }

            if (currentTextIndex === 0) return; // Don't delete if on first character.

            // 🎁 Exception for moving backwards at the start of a word-wrapped line.
            if (cursorTextIndex === undefined && currentTextIndex > 0) {
              this.text =
                this.text.slice(0, currentTextIndex - 1) +
                this.text.slice(currentTextIndex);
              prompt.cursor = {
                ...prompt.textToCursorMap[currentTextIndex - 1],
              };
            }

            if (cursorTextIndex >= 0) {
              this.text =
                this.text.slice(0, cursorTextIndex) +
                this.text.slice(cursorTextIndex + 1);

              let cursor = prompt.textToCursorMap[cursorTextIndex - 1];

              if (!cursor) {
                if (prompt.posHasVisibleCharacter()) {
                  cursor = prompt.textToCursorMap[cursorTextIndex];
                } else {
                  cursor = { x: 0, y: 0 };
                }
              }

              if (cursor) {
                prompt.cursor = { ...cursor };
                if (cursorTextIndex > 0 && !hasNewLine) {
                  if (!prompt.wrapped(cursorTextIndex)) prompt.forward();
                }
              } else {
                prompt.crawlBackward();
              }
            }
          }
        }

        if (e.key === "Escape") {
          // console.log("💻 Activation:", this.activate, typeof this);
          this.activate(this);
          $.send({ type: "keyboard:open" });
          this.text = "";
          $.send({ type: "keyboard:text:replace", content: { text: "" } });
          this.#prompt.cursor = { x: 0, y: 0 };
        }

        // Move backwards through history stack.
        if (e.key === "ArrowUp" && !this.skipHistory) {
          // TODO: Check to see if this is the first history traversal,
          //       and store the current text if it is...
          const history = (await store.retrieve(this.key)) || [""];
          if (this.#prehistory === undefined) this.#prehistory = this.text;

          this.historyDepth += 1;
          if (this.historyDepth === history.length) {
            this.historyDepth = -1;
          }

          if (this.historyDepth === -1) {
            this.text = this.#prehistory;
          } else {
            this.text = history[this.historyDepth] || "";
          }

          this.snap();

          $.send({
            type: "keyboard:text:replace",
            content: { text: this.text },
          });
          this.selection = null;
        }

        // ... and forwards.
        if (e.key === "ArrowDown" && !this.skipHistory) {
          const history = (await store.retrieve(this.key)) || [""];
          if (this.#prehistory === undefined) this.#prehistory = this.text;

          this.historyDepth -= 1;
          if (this.historyDepth < -1) this.historyDepth = history.length - 1;

          if (this.historyDepth === -1) {
            this.text = this.#prehistory;
          } else {
            this.text = history[this.historyDepth] || "";
          }

          this.snap();
          $.send({
            type: "keyboard:text:replace",
            content: { text: this.text },
          });
          this.selection = null;
        }
      }

      if (e.key !== "Enter" && e.key !== "`") {
        // Only manage Enter button disabled state when TextInput is active
        if (this.canType) {
          if (this.text.length > 0) {
            this.enter.btn.disabled = false;
            this.runnable = true;
          } else {
            this.enter.btn.disabled = true;
            this.runnable = false;
          }
        }
      }

      if (e.key === "Enter" && !this.skipEnter) {
        if (e.shift) {
          // ✏️ Make a new line while editing.
          const pos = this.#prompt.textPos();
          const char = this.text[pos];
          if (
            pos === undefined ||
            (char?.charCodeAt(0) === 10 && pos === this.text.length - 1)
          ) {
            this.text += `\n`;
            this.#prompt.newLine();
            this.snap();
          } else {
            const hasVis = this.#prompt.posHasVisibleCharacter();

            // Check to see if the cursor is at the start of a word breaked word
            // and if it is, then add another line.
            let insert = "\n";
            let wrapped = false;
            if (this.#prompt.wrapped(pos)) {
              wrapped = true;
              insert += "\n"; // Add an extra line if on a wrapped word.
            }
            this.text = this.text.slice(0, pos) + insert + this.text.slice(pos);

            if (hasVis && !wrapped) {
              this.#prompt.cursor = { ...this.#prompt.textToCursorMap[pos] };
            } else {
              this.#prompt.cursor.y += 1;
            }
          }
          $.send({
            type: "keyboard:text:replace",
            content: { text: this.text /*cursor: this.#prompt.textPos()*/ },
          });
        } else if (this.runnable) {
          if (this.text.trim().length > 0) {
            // 💻 Execute a command!
            if (!this.mute) {
              sound.synth({
                type: "sine",
                tone: 850,
                attack: 0.1,
                decay: 0.96,
                volume: 0.65,
                duration: 0.005,
              });
            }
            await this.run(store);
            // Deactivate directly after sending message via keyboard Enter
            deactivate(this);
          }
        } else if (!this.canType && !this.#edgeCancelled) {
          activate(this);
        }
      }

      this.#ensureBlink();
      this.blink?.flip(true);
      this.showBlink = true;
    }

    // Handle activation / focusing of the input
    // (including os-level software keyboard overlays)
    if (e.is("keyboard:open") && !this.#lock && !this.#edgeCancelled) {
      // Only activate via keyboard:open if we haven't manually deactivated recently
      // and there's no active touch interaction
      const timeSinceManualDeactivation = Date.now() - this.#manualDeactivationTime;
      const timeSinceManualActivation = Date.now() - this.#manualActivationTime;
      if (!this.#manuallyDeactivated || this.#activatingPress || timeSinceManualDeactivation > 100) {
        activate(this);
      }
    }
    if (e.is("keyboard:close") && !this.#lock) {
      // Don't process if already deactivated
      if (!this.canType) {
        return;
      }
      
      // Don't deactivate if we're preventing deactivation (during command execution)
      if (this.#preventDeactivation || this._preventDeactivation) {
        return;
      }
      
      // Only deactivate via keyboard:close if we haven't recently manually activated via touch
      const timeSinceManualDeactivation = Date.now() - this.#manualDeactivationTime;
      const timeSinceManualActivation = Date.now() - this.#manualActivationTime;
      
      // Don't close if Enter button just activated the TextInput
      if (this._enterButtonActivation) {
        return;
      }
      
      // Increase the threshold for manual activation to 500ms to account for command execution time
      if ((!this.#manuallyDeactivated || timeSinceManualDeactivation > 100) && (!this.#manuallyActivated || timeSinceManualActivation > 500)) {
        deactivate(this);
      }
    }

    if (
      e.is("touch") &&
      !this.#lock &&
      //!this.inputStarted &&
      !this.canType &&
      !this.backdropTouchOff &&
      (this.copy.btn.disabled === true || !this.copy.btn.box.contains(e)) &&
      (this.paste.btn.disabled === true || !this.paste.btn.box.contains(e))
    ) {
      this.#activatingPress = true;
      this.#edgeCancelled = false; // Reset edge cancellation on new touch
      // Note: Don't reset #manuallyDeactivated here - let the time-based logic handle it
      $.send({ type: "keyboard:unlock" });
      if (!this.mute) {
        sound.synth({
          type: "sine",
          tone: 300,
          attack: 0.1,
          decay: 0.96,
          volume: 0.5,
          duration: 0.01,
        });
      }
    }

    // Begin the prompt input mode / leave the splash.
    function activate(ti) {
      ti.activatedOnce = true;

      if (ti.canType) {
        // (This redundancy check is because this behavior is tied to
        // keyboard open and close events.)
        return;
      }

      // Note: Don't reset #manuallyDeactivated here - let the time-based logic handle it

      ti.activated?.($, true);
      ti.#activatingPress = false;
      // ti.enter.btn.down = false;
      if (ti.text.length > 0) {
        ti.copy.btn.disabled = true;
        ti.copy.btn.removeFromDom($, "copy");
      }
      ti.canType = true;

      if (ti.#lastUserText.length > 0) {
        ti.text = ti.#lastUserText;
        ti.runnable = true;
        ti.paste.btn.disabled = false;
      } else {
        if (ti.#lastPrintedText) {
          ti.blank("blink");
        }

        // ti.enter.btn.disabled = true;
        ti.runnable = false; // Explicitly set runnable to false when no text
        ti.paste.btn.disabled = false;
      }
      
      //ti.inputStarted = true;
      $.act("text-input:editable");

      // Ensure keyboard is unlocked and opened for mobile devices
      $.send({ type: "keyboard:unlock" });
      $.send({ type: "keyboard:open" }); // Necessary for desktop and mobile.
      


      if (!ti.mute) {
        sound.synth({
          type: "sine",
          tone: 300,
          attack: 0.1,
          decay: 0.96,
          volume: 0.5,
          duration: 0.01,
        });
      }
    }

    this.activate = activate;

    // Leave the prompt input mode.
    function deactivate(ti) {
      // Prevent deactivation if the flag is set
      if (ti.#preventDeactivation || ti._preventDeactivation) {
        return;
      }
      
      if (ti.canType === false) {
        // Assume we are already deactivated.
        // (This redundancy check is because this behavior is tied to
        // keyboard open and close events.)
        return;
      }

      ti.activated?.($, false);

      ti.enter.btn.disabled = false; // Always enable Enter button when deactivated so user can reactivate
      ti.paste.btn.disabled = false;
      ti.canType = false;
      ti.runnable = false;
      ti.#lastUserText = ti.text;
      ti.backdropTouchOff = false;

      ti.text = ti.#lastPrintedText || ti.text;

      if (ti.#lastPrintedText.length > 0 && ti.commandSentOnce) {
        ti.copy.btn.disabled = false;
        ti.#coatedCopy = ti.#coatCopy(ti.text); // Wrap text to be copied.

        ti.paste.btn.disabled = true; // Disable paste button.
        ti.paste.btn.removeFromDom($, "paste");
      }

      $.act("text-input:uneditable");
      needsPaint();

      if (!ti.mute) {
        sound.synth({
          type: "sine",
          tone: 250,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      }

      ti.mute = false; // Always unmute on deactivation,
      //                  for `field`. 23.12.02.00.43
    }

    if (
      e.is("touch") &&
      ((this.enter.btn.disabled === false && this.enter.btn.box.contains(e)) ||
        (this.copy.btn.disabled === false && this.copy.btn.box.contains(e)) ||
        (this.paste.btn.disabled === false && this.paste.btn.box.contains(e)))
    ) {
      this.backdropTouchOff = true;
    }

    if (e.is("lift")) {
      // Handle shifting reset first, before other lift logic
      if (this.shifting) {
        this.#moveDeltaX = 0;
        this.shifting = false;
        this.#recentlyShifting = true; // Track that we just finished character sliding
        
        // Reset the recently shifting flag after a short delay
        setTimeout(() => {
          this.#recentlyShifting = false;
        }, 50);
      }
      
      // Store the current backdropTouchOff state before resetting it
      const shouldPreventActivation = this.backdropTouchOff;
      this.backdropTouchOff = false;
      
      // Unlock keyboard on lift (unless locked or a button was touched)
      // Don't unlock if a button interaction was happening - let bios handle it
      if (!this.#lock && !shouldPreventActivation) {
        $.send({ type: "keyboard:unlock" });
      }
      
      // Process activation for inactive TextInput
      if (!this.canType) {
        // Only process lift if we had an activating press (prevents orphaned lifts)
        if (this.#activatingPress) {
          // Check if we should activate BEFORE setting activatingPress to false
          // But only if edge cancellation didn't happen AND backdrop touch shouldn't be prevented
          if (!this.#edgeCancelled && !shouldPreventActivation) {
            this.#manuallyActivated = true; // Set flag to block unwanted keyboard events
            this.#manualActivationTime = Date.now();
            activate(this);
          } 

          this.#activatingPress = false;
        }
        // Don't reset #edgeCancelled here - let it persist to prevent keyboard events from activating
      } else {
        // Handle deactivation for active TextInput
        // Don't deactivate if lift is over Enter button and button is down (push is about to occur)
        const isOverEnterButton = (this.enter.btn.disabled === false && this.enter.btn.box.contains(e));
        const enterButtonIsDown = this.enter.btn.down;
        
        // Check if lift is over any interactive element
        const isOverInteractive = (
          (this.copy.btn.disabled === false && this.copy.btn.box.contains(e)) ||
          (this.paste.btn.disabled === false && this.paste.btn.box.contains(e)) ||
          isOverEnterButton
        );
        
        // Don't deactivate if:
        // 1. Over interactive elements OR if over active enter button
        // 2. Just finished character sliding (recentlyShifting)
        if (!isOverInteractive || (isOverEnterButton && enterButtonIsDown)) {
          if (!isOverInteractive && !this.#recentlyShifting) {
            this.#manuallyDeactivated = true;
            this.#manualDeactivationTime = Date.now();
            deactivate(this);
          }
        }
      }
    }

    // UI Button Actions
    if (!this.#lock) {
      // Enter Button...
      if (
        e.is("draw") &&
        this.enter.btn.disabled === false &&
        this.enter.btn.box.contains(e) &&
        !this.enter.btn.down
      ) {
        $.send({ type: "keyboard:lock" });
      }

      // Copy Button...
      if (
        (e.is("draw") || e.is("touch")) &&
        this.copy.btn.disabled === false &&
        this.copy.btn.box.contains(e) // &&
      ) {
        $.send({ type: "keyboard:lock" });
      }

      // Paste button...
      if (
        (e.is("draw") || e.is("touch")) &&
        this.paste.btn.disabled === false &&
        this.paste.btn.box.contains(e) // &&
      ) {
        $.send({ type: "keyboard:lock" });
      }

      // 🔲 Enter
      this.enter.btn.act(e, {
        down: () => {
          $.send({ type: "keyboard:unlock" }); // Unlock keyboard for mobile
          if (!this.mute) {
            sound.synth({
              type: "sine",
              tone: 600,
              attack: 0.1,
              decay: 0.99,
              volume: 0.75,
              duration: 0.001,
            });
          }
          needsPaint();
        },
        scrub: () => {
          // Silent scrubbing
        },
        push: async () => {
          // Prevent race conditions by checking if we're already processing
          if (this.#lock) {
            return;
          }
          
          // If the TextInput is not active but has text that should be sent
          if (!this.canType && this.text.trim().length > 0) {
            this.#manuallyActivated = true;
            this.#manualActivationTime = Date.now();
            activate(this);
            // Run command immediately after activation
            if (this.runnable && this.text.trim().length > 0) {
              await this.run(store);
              deactivate(this);
            }
          } else if (this.runnable && this.text.trim().length > 0) {
            this.#manuallyActivated = true;
            this.#manualActivationTime = Date.now();
            await this.run(store);
            // Deactivate directly after sending message
            this._enterHandledMessage = true; // Flag to prevent redundant keyboard:close
            deactivate(this);
            // Clear the flag after a short delay
            setTimeout(() => {
              this._enterHandledMessage = false;
            }, 200);
          } else if (this.runnable && this.text.trim().length === 0 && this.closeOnEmptyEnter) {
            deactivate(this);
          } else {
            // Reset edge cancellation for Enter button activation
            this.#edgeCancelled = false;
            
            this.#manuallyActivated = true;
            this.#manualActivationTime = Date.now();
            activate(this);
          }
        },
        cancel: () => {
          $.send({ type: "keyboard:lock" });
          needsPaint();
        },
        rollover: (btn) => {
          if (btn) $.send({ type: "keyboard:unlock" });
          needsPaint();
        },
        rollout: () => {
          $.send({ type: "keyboard:lock" });
          needsPaint();
        },
      });

      // 🔲 Copy
      this.copy.btn.act(e, {
        down: () => {
          needsPaint();
          if (!this.mute) {
            sound.synth({
              type: "sine",
              tone: 600,
              attack: 0.1,
              decay: 0.99,
              volume: 0.75,
              duration: 0.001,
            });
          }
        },
        push: () => {
          if (!this.mute) {
            sound.synth({
              type: "sine",
              tone: 800,
              attack: 0.1,
              decay: 0.99,
              volume: 0.75,
              duration: 0.005,
            });
          }
          needsPaint();
        },
        cancel: () => needsPaint(),
      });

      // 🔲 Paste
      this.paste.btn.act(e, {
        down: () => {
          needsPaint();
          if (!this.mute) {
            sound.synth({
              type: "sine",
              tone: 600,
              attack: 0.1,
              decay: 0.99,
              volume: 0.75,
              duration: 0.001,
            });
          }
        },
        push: () => {
          if (!this.mute) {
            sound.synth({
              type: "sine",
              tone: 800,
              attack: 0.1,
              decay: 0.99,
              volume: 0.75,
              duration: 0.005,
            });
          }
          needsPaint();
        },
        cancel: () => needsPaint(),
      });
    }

    // ✂️ Copy to a user's clipboard.
    if (e.name?.startsWith("clipboard:copy")) {
      const copied = e.is("clipboard:copy:copied");
      if (debug) {
        copied
          ? "📋 Copy: Copied 🙃"
          : console.warn("📋 Copy: Failed ⚠️");
      }

      this.copy.txt = copied
        ? this.scheme.buttons?.copy?.copied || "Copied"
        : this.scheme.buttons?.copy?.failed || "Failed";
      this.#copyPasteScheme = this.#buildCopyPasteScheme(); // Greyed out.

      needsPaint();
      clearTimeout(this.#copyPasteTimeout);
      this.#copyPasteTimeout = setTimeout(() => {
        this.copy.btn.disabled = false;
        this.copy.txt = this.scheme.buttons?.copy?.label || "Copy";
        this.#copyPasteScheme = undefined;
        needsPaint();
      }, 500);
    }

    // 🗞️ Paste UI signal.
    if (e.name?.startsWith("clipboard:paste")) {
      let label;
      if (e.is("clipboard:paste:pasted")) {
        if (debug) "📋 Paste: Pasted 🙃";
        label = this.scheme.buttons?.paste?.pasted || "Pasted";
      } else if (e.is("clipboard:paste:pasted:empty")) {
        if (debug) console.warn("📋 Paste: Empty 👐️");
        label = this.scheme.buttons?.paste?.empty || "Empty";
      } else {
        if (debug) console.warn("📋 Paste: Failed ⚠️");
        label = this.scheme.buttons?.paste?.failed || "Failed";
      }

      this.paste.txt = label;
      this.#copyPasteScheme = this.#buildCopyPasteScheme(); // Greyed out.
      needsPaint();
      clearTimeout(this.#copyPasteTimeout);
      this.#copyPasteTimeout = setTimeout(() => {
        this.paste.btn.disabled = false;
        this.paste.txt = this.scheme.buttons?.paste?.label || "Paste";
        this.#copyPasteScheme = undefined;
        needsPaint();
      }, 500);
    }

    // This should only be possible when the text box is locked, unless
    // it's the first activation.
    if (
      e.is("prompt:text:replace") &&
      (!this.activatedOnce || this.#lock === false)
    ) {
      this.text = e.text;
      this.#lastUserText = e.text;
      this.#prompt.snapTo(this.text.slice(0, e.cursor));
      // this.runnable = true;
      this.#ensureBlink();
      this.blink?.flip(true);
      this.selection = null;

      if (this.text.length > 0) {
        this.enter.btn.disabled = false;
        this.runnable = true;
      } else {
        this.enter.btn.disabled = true;
        this.runnable = false;
      }

      if (this.#prehistory !== undefined) this.#prehistory = this.text;
    }

    if (e.is("prompt:text:cursor") && this.#lock === false) {
      if (e.cursor === this.text.length) {
        // this.snap();
        this.#prompt.snapTo(this.text);
      } else {
        this.#prompt.cursor = { ...this.#prompt.textToCursorMap[e.cursor] };
      }

      if (e.start !== undefined && e.end !== undefined) {
        this.selection = [e.start, e.end];
      } else {
        this.selection = null;
      }

      this.#ensureBlink();
      this.blink?.flip(true);
    }

    if (e.is("touch") && !this.#lock) {
      this.#ensureBlink();
      this.blink?.flip(true);
      this.#recentlyShifting = false; // Reset the recently shifting flag on new touch
    }

    if (
      e.is("draw") &&
      !this.#lock &&
      this.canType &&
      !this.enter.btn.down &&
      !this.paste.btn.down
    ) {
      if (!this.shifting) {
        $.send({ type: "keyboard:lock" });

        this.shifting = true;
        this.backdropTouchOff = true;
      }

      if (
        (this.#moveDeltaX > 0 && e.delta.x < 0) ||
        (this.#moveDeltaX < 0 && e.delta.x > 0)
      ) {
        this.#moveDeltaX = 0; // Reset delta on every directional change.
      }

      this.#moveDeltaX += e.delta.x; // Add up the deltas.

      while (this.#moveDeltaX <= -this.#moveThreshold) {
        this.#moveDeltaX += this.#moveThreshold;
        this.#prompt.crawlBackward();
        this.selection = null;
        $.send({ type: "keyboard:cursor", content: -1 });
        
        // Play character movement click sound
        if (!this.mute) {
          sound.synth({
            type: "sine",
            tone: 800,
            attack: 0.01,
            decay: 0.95,
            volume: 0.25,
            duration: 0.008,
          });
        }
      }

      while (this.#moveDeltaX >= this.#moveThreshold) {
        this.#moveDeltaX -= this.#moveThreshold;
        this.#prompt.crawlForward();
        this.selection = null;
        if (this.prompt.textPos() === undefined) {
          $.act("textinput:shift-right:empty"); // Send a signal when we shift
          // to the right past the edge of the text, for autocomplete
          // implementations in pieces that use `TextInput`.
        }
        $.send({ type: "keyboard:cursor", content: 1 });
        
        // Play character movement click sound
        if (!this.mute) {
          sound.synth({
            type: "sine",
            tone: 800,
            attack: 0.01,
            decay: 0.95,
            volume: 0.25,
            duration: 0.008,
          });
        }
      }

      this.#ensureBlink();
      this.blink?.flip(true);
    }
  }
}

// Manages the scale / wrapping of text and the interaction of a cursor.
// (Just for rendering of `Text`)
class Prompt {
  top = 0;
  left = 0;

  wrap = "char"; // auto-wrap setting, could also be "word".
  scale = 1;

  letterWidth; // Taken from the typeface's block sizing.
  letterHeight;
  typeface; // Reference to the typeface for proportional spacing

  colWidth = 48; // Maximum character width of each line before wrapping.

  cursor = { x: 0, y: 0 };
  gutter; // A y-position at the end of the colWidth.

  lineBreaks = []; // Legacy?

  cursorToTextMap = {}; // Keep track of text data in relationship to whitespace.
  textToCursorMap = [];
  wrappedWordIndices = []; // Keep track of word wrapped indices after
  //                           each mapping.

  #mappedTo = ""; // Text that has been mapped.

  constructor(
    top = 0,
    left = 0,
    wrap,
    colWidth = 48,
    lineSpacing = 0,
    typeface,
  ) {
    this.typeface = typeface; // Store typeface reference for proportional spacing
    this.letterWidth = typeface.blockWidth * this.scale;
    this.letterHeight = typeface.blockHeight * this.scale + lineSpacing;
    this.top = top;
    this.left = left;
    this.wrap = wrap;
    this.colWidth = colWidth;
    this.gutter = this.colWidth * this.letterWidth;
  }

  // Snap the cursor to the end of a text.
  snapTo(text) {
    if (text[text.length - 1]) {
      this.cursor = { ...this.textToCursorMap[text.length - 1] };
      if (text[text.length - 1].charCodeAt(0) !== 10) this.forward(); // Move ahead one space after the end.
    } else {
      this.cursor = { x: 0, y: 0 };
    }
  }

  // Generate text map for rendering and UI operations.
  mapTo(text) {
    // Begin the cursor / text-wrapping crawl.
    this.#mappedTo = text;
    this.cursorToTextMap = {};
    this.textToCursorMap = [];
    this.wrappedWordIndices = [];
    const cursor = { x: 0, y: 0 }; // Wrap and map the text either by character or word.
    // (Word wrapping is complex and skips text indices for invisible
    //  characters and in some edge cases with line breaks)
    if (this.wrap === "char") {
      let textIndex = 0;
      let brokeLine = false;

      // Use Array.from to properly handle Unicode code points
      const characters = Array.from(text);

      for (let c = 0; c < characters.length; c += 1) {
        const char = characters[c];
        const newLine = char.charCodeAt(0) === 10;

        if (c === 0) {
          if (newLine) {
            this.newLine(cursor);
            brokeLine = true;
          }
          this.#updateMaps(text, textIndex, cursor); // Update cursor<->text indexing.
          continue;
        }

        if (newLine) {
          this.newLine(cursor);
          brokeLine = true;
        } else {
          !brokeLine ? this.forward(cursor) : (brokeLine = false);
        }
        textIndex += 1;
        this.#updateMaps(text, textIndex, cursor); // Update cursor<->text indexing.
      }
    } else if (this.wrap === "word") {
      let textIndex = 0;
      let brokeLine = false;
      let wordStart = false;
      let wordCount = 0;

      // Use Array.from to properly handle Unicode code points
      const characters = Array.from(text);

      for (let c = 0; c < characters.length; c += 1) {
        const char = characters[c];
        let newLine = char.charCodeAt(0) === 10;

        // First character...
        if (c === 0) {
          if (newLine) {
            this.newLine(cursor);
            brokeLine = true;
          }
          this.#updateMaps(text, textIndex, cursor);
          if (!newLine && char !== " ") {
            wordStart = true;
            wordCount += 1;
          }
          continue;
        }

        //if (!newLine && char !== " ") {
        if (!newLine && char !== " ") {
          if (!wordStart) {
            wordStart = true;
            wordCount += 1;
            let len = 0;
            for (let i = c; i < characters.length; i += 1) {
              const char = characters[i];
              if (char !== " " && char.charCodeAt(0) !== 10) {
                len += 1;
              } else {
                break;
              }
            }

            if (cursor.x + len >= this.colWidth - 1) {
              if (!this.posHasNewLine(cursor)) {
                this.newLine(cursor);
                brokeLine = true;
                this.wrappedWordIndices.push(c);
              }
            }
          }
        } else {
          wordStart = false;
        } // Create a line break if a line will begin with a space and we're
        // not on a space.
        if (
          char === " " &&
          cursor.x + 1 === this.colWidth - 1 &&
          characters[textIndex] !== " "
        ) {
          newLine = true;
        }

        if (newLine) {
          this.newLine(cursor);
          brokeLine = true;
        } else {
          !brokeLine ? this.forward(cursor) : (brokeLine = false);
        }

        textIndex += 1;
        this.#updateMaps(text, textIndex, cursor);
      }
    }
  }

  // Lookup to check if the word at the beginning of this index was
  // word-wrapped.
  wrapped(index) {
    if (this.wrap !== "word") return false; // Make sure word wrap is on.
    return this.wrappedWordIndices.includes(index);
  }

  #updateMaps(text, textIndex, cursor = this.cursor) {
    const char = text[textIndex];
    const newLine = char.charCodeAt(0) === 10;
    this.textToCursorMap[textIndex] = { ...cursor };
    let key = `${cursor.x}:${cursor.y}`;
    if (newLine) key = key + ":\\n";
    this.cursorToTextMap[key] = textIndex;
  }

  // Get the current text index given a cursor position.
  textPos(cursor = this.cursor) {
    if (this.textToCursorMap.length === 0) {
      return 0;
    } else {
      // First check for any preceeding new line characters...
      const key = `${cursor.x}:${cursor.y}`;
      let pos = this.cursorToTextMap[key];
      if (pos === undefined) pos = this.cursorToTextMap[key + ":\\n"];
      return pos;
    }
  }

  // Determine whether a cursor has a visible character in the map.
  posHasVisibleCharacter(cursor = this.cursor) {
    return this.cursorToTextMap[`${cursor.x}:${cursor.y}`] !== undefined;
  }

  // Determine whether there is an invisible new line character
  // under the cursor in the map.
  posHasNewLine(cursor = this.cursor) {
    return this.cursorToTextMap[`${cursor.x}:${cursor.y}:\\n`] !== undefined;
  }

  // Flatten the coordinates of the cursor to return a linear value.
  // (Does not necessarily match text, due to line breaks, etc.)
  get index() {
    const x = this.cursor.x;
    const y = this.cursor.y;
    const cols = this.colWidth;
    const lineBreaks = y; // Number of line breaks before the current row
    return y * (cols + 1) + x - lineBreaks;
  }

  // Caluclate the screen x, y position of the top left of the cursor.
  // (Also include the width and height of the block.)
  pos(cursor = this.cursor, bh = false) {
    const y = this.left + cursor.y * this.letterHeight;
    
    // Check if we need proportional spacing
    const isProportional = this.typeface?.data?.proportional === true ||
                           this.typeface?.data?.bdfFont === "MatrixChunky8" ||
                           this.typeface?.name === "MatrixChunky8" ||
                           !!this.typeface?.data?.advances;
    
    let x;
    let w;
    
    if (isProportional && this.#mappedTo && typeof this.typeface?.getAdvance === "function") {
      // Calculate x position by summing advance widths of all characters on this line
      // Find characters on the current line up to cursor.x
      x = this.top;
      
      // Get all characters mapped to this line (y position)
      let charsOnLine = 0;
      for (const [key, textIdx] of Object.entries(this.cursorToTextMap)) {
        const [kx, ky] = key.split(":").map(v => parseInt(v));
        if (ky === cursor.y && kx < cursor.x && !key.includes("\\n")) {
          charsOnLine++;
        }
      }
      
      // Sum the advance widths of characters up to cursor position
      // Look through textToCursorMap to find characters on this line before cursor.x
      for (let i = 0; i < this.#mappedTo.length; i++) {
        const mappedPos = this.textToCursorMap[i];
        if (mappedPos && mappedPos.y === cursor.y && mappedPos.x < cursor.x) {
          const char = this.#mappedTo[i];
          if (char && char.charCodeAt(0) !== 10) {
            const advance = this.typeface.getAdvance(char);
            x += (typeof advance === "number" ? advance : this.letterWidth) * this.scale;
          }
        }
      }
      
      // Width is the advance of the current character (or default letterWidth)
      const cursorTextIdx = this.textPos(cursor);
      if (cursorTextIdx !== undefined && this.#mappedTo[cursorTextIdx]) {
        const curChar = this.#mappedTo[cursorTextIdx];
        const advance = this.typeface.getAdvance(curChar);
        w = (typeof advance === "number" ? advance : this.letterWidth) * this.scale;
      } else {
        w = this.letterWidth;
      }
    } else {
      // Original monospace calculation
      x = this.top + cursor.x * this.letterWidth;
      w = this.letterWidth;
    }
    
    return {
      x,
      y,
      w,
      h: this.letterHeight,
    };
  }

  // Move the cursor forward, optionally input an override cursor.
  forward(cursor = this.cursor, amount = 1) {
    repeat(amount, () => {
      cursor.x = (cursor.x + 1) % (this.colWidth - 1);
      if (cursor.x === 0) cursor.y += 1;
    });
    return cursor;
  }

  // Move the cursor forward only by the mapped text.
  crawlForward() {
    if (this.#mappedTo.length === 0) return;

    const back = this.backward({ ...this.cursor });
    const backIndex = this.textPos(back);
    const startIndex = this.textPos();

    if (
      backIndex === this.#mappedTo.length ||
      (startIndex === this.#mappedTo.length - 1 &&
        !this.posHasVisibleCharacter())
    ) {
      return; // We are at the end.
    }

    if (backIndex !== this.#mappedTo.length - 1) {
      // Check to see if the next character is a new line and
      // only move forward on the x if it is.
      if (
        this.#mappedTo[startIndex + 1]?.charCodeAt(0) === 10 &&
        this.posHasVisibleCharacter()
      ) {
        this.cursor.x += 1;
      } else {
        // Otherwise move forward and jump through potential word wrapping.
        this.forward();
        // Skip any undefined / wrapped sections.
        if (startIndex !== this.#mappedTo.length - 1) {
          while (this.textPos() === undefined) {
            this.forward();
          }
        }
      }
    } else if (startIndex === 0) this.forward();
  }

  // Move the cursor backward only by the mapped text.
  crawlBackward() {
    const back = this.backward({ ...this.cursor });
    let backIndex = this.textPos(back);
    const currentIndex = this.textPos();

    if (backIndex === undefined) {
      // Check for new line character.
      if (
        this.posHasNewLine() &&
        currentIndex <= 1 &&
        this.#mappedTo[currentIndex].charCodeAt(0) === 10
      ) {
        this.cursor.y -= 1;
        return;
      } else {
        if (this.posHasNewLine()) {
          const backupAmount = this.posHasVisibleCharacter() ? 2 : 1;
          this.cursor = {
            ...this.textToCursorMap[currentIndex - backupAmount],
          };
          if (this.posHasVisibleCharacter()) this.forward();
          return;
        } else {
          // Otherwise back up as needed.
          while (backIndex === undefined) {
            this.backward();
            backIndex = this.textPos(this.backward(back));
          }
        }
      }
    }
    this.backward();
  }

  // Move cursor backward, with optional override cursor.
  backward(cursor = this.cursor) {
    if (cursor.x === 0) {
      if (cursor.y > 0) {
        cursor.x = this.colWidth - 2;
        cursor.y -= 1;
      }
    } else {
      cursor.x -= 1;
    }
    return cursor;
  }

  // Create and track a cursor line break.
  newLine(cursor = this.cursor) {
    cursor.y += 1;
    cursor.x = 0;
  }
}

export { Typeface, TextInput, Prompt, clearGlyphCache };
