// Stample, 2025.1.28.04.11.21.170
// Spread a sample across some pats.

/* 📝 Notes
  - [] Paint a line from each pen start point to the current point.
  - [] Add loop toggle / switch?
  - [] Add a subtle attack and decay to sample playback. 
  - [] Wait until mouse moves one delta y pixel to determine sample playback
        direction.
  - [] Add ability to shift / scroll start and end points.
  - [] Automatically dip the max volume if multiple samples are playing.
  - [] Add visual printing / stamping of pixel data and loading of that
       data.
  - [] Support `stample $code` to sample pixels from a running KidLisp piece
  + Done
  - [x] Add positional swiping.
  - [x] Add `paintSound` to the disk library / make a really good abstraction for
        that.
  - [x] Add live pitch shifting / speed.
  - [x] Show the state of the microphone connection, recording and disconnect
         process.
  - [x] Show a tiny waveform in the record button for live feedback.
  - [x] Record a sample and spread it automatically.
  - [x] Draw the waveform over the buttons.
 - [x] Spread out a predefined sample.
    - [x] Add start and end trimming to sound.play();
 */

import {
  encodeSampleToBitmap,
  decodeBitmapToSample,
  loadPaintingAsAudio,
} from "../lib/pixel-sample.mjs";

const { abs, min, max, floor, ceil } = Math;

// Layout Constants
const BUTTON_LABEL_CONNECTING = "Wait...";
const maxPats = 10;

// Responsive Layout Thresholds
const COMPACT_HEIGHT = 160;
const NARROW_WIDTH = 180;
const BITMAP_BESIDE_MIN_WIDTH = 280;

// Layout Zone Defaults
const TOP_BAR_HEIGHT = 24;
const BOTTOM_BAR_HEIGHT = 36;
const COMPACT_TOP_BAR = 18;
const COMPACT_BOTTOM_BAR = 28;

// Bitmap Preview Sizing
const BITMAP_MIN_SIZE = 56;
const BITMAP_MAX_SIZE = 120;
const BITMAP_MARGIN = 8;
const BITMAP_BTN_HEIGHT = 18;
const BITMAP_BTN_GAP = 4;

// Strip Button Constraints
const MIN_STRIP_WIDTH = 40;
const MIN_STRIP_HEIGHT = 20;

// Layout Cache
let layoutCache = { key: null, metrics: null };

let loop = true; // Global setting.

// ─────────────────────────────────────────────────────────────────────────────
// 🎨 RESPONSIVE LAYOUT SYSTEM (inspired by notepat.mjs)
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Computes all layout metrics based on screen size and state.
 * Returns a metrics object that can be used for positioning all UI elements.
 */
function getLayoutMetrics(screen, { hasBitmap = false, isRecording = false, patCount = 1 } = {}) {
  const isCompact = screen.height < COMPACT_HEIGHT;
  const isNarrow = screen.width < NARROW_WIDTH;
  const isLandscape = screen.width > screen.height;
  
  // Calculate zone heights
  const topBar = isCompact ? COMPACT_TOP_BAR : TOP_BAR_HEIGHT;
  const bottomBar = isCompact ? COMPACT_BOTTOM_BAR : BOTTOM_BAR_HEIGHT;
  const availableHeight = max(0, screen.height - topBar - bottomBar);
  
  // ─────────────────────────────────────────────────────────────────────────
  // RECORDING MODE: Full-screen centered layout
  // ─────────────────────────────────────────────────────────────────────────
  if (isRecording) {
    const margin = isCompact ? 12 : 20;
    const stopBtnH = isCompact ? 32 : 44;
    const stopBtnW = isCompact ? 80 : 110;
    
    const previewW = min(screen.width - margin * 2, 320);
    const previewH = max(60, screen.height - topBar - stopBtnH - margin * 3);
    const previewX = floor((screen.width - previewW) / 2);
    const previewY = topBar + margin;
    
    return {
      mode: 'recording',
      isCompact,
      topBar,
      bottomBar,
      // Recording preview
      previewX,
      previewY,
      previewW,
      previewH,
      // Stop button
      stopBtnX: floor((screen.width - stopBtnW) / 2),
      stopBtnY: screen.height - stopBtnH - margin,
      stopBtnW,
      stopBtnH,
    };
  }
  
  // ─────────────────────────────────────────────────────────────────────────
  // NORMAL MODE: Determine bitmap position and strip layout
  // ─────────────────────────────────────────────────────────────────────────
  
  // Decide if bitmap goes beside strips or below (portrait stacking)
  const canFitBitmapBeside = isLandscape && screen.width >= BITMAP_BESIDE_MIN_WIDTH;
  const bitmapBeside = hasBitmap && canFitBitmapBeside;
  
  // Calculate bitmap size (responsive to available space)
  let bitmapSize = 0;
  let bitmapColumnW = 0;
  let bitmapX = 0;
  let bitmapY = 0;
  let bitmapBtnY = 0;
  let bitmapBtnW = 0;
  
  if (hasBitmap) {
    if (bitmapBeside) {
      // Bitmap column on the right side
      const maxBitmapH = availableHeight - BITMAP_BTN_HEIGHT * 2 - BITMAP_BTN_GAP * 2 - BITMAP_MARGIN * 2;
      bitmapSize = min(BITMAP_MAX_SIZE, max(BITMAP_MIN_SIZE, floor(maxBitmapH)));
      bitmapColumnW = bitmapSize + BITMAP_MARGIN * 2;
      bitmapX = screen.width - bitmapSize - BITMAP_MARGIN;
      bitmapY = topBar + BITMAP_MARGIN;
      bitmapBtnW = bitmapSize;
      bitmapBtnY = bitmapY + bitmapSize + BITMAP_BTN_GAP;
    } else {
      // Bitmap overlaid in bottom-right corner (portrait/narrow mode)
      // Calculate record button dimensions first to avoid overlap
      const recordBtnW = isCompact ? 52 : 68;
      const recordBtnRightEdge = BITMAP_MARGIN + recordBtnW + BITMAP_MARGIN;
      
      // Calculate available width for bitmap (avoiding record button)
      const availableBitmapWidth = screen.width - recordBtnRightEdge - BITMAP_MARGIN;
      bitmapSize = min(BITMAP_MAX_SIZE, max(BITMAP_MIN_SIZE, floor(min(screen.width * 0.35, availableBitmapWidth))));
      bitmapColumnW = 0; // No column, overlaid
      bitmapX = screen.width - bitmapSize - BITMAP_MARGIN;
      
      // Calculate total bitmap UI height (bitmap + 2 buttons + gaps)
      const totalBitmapUIHeight = bitmapSize + BITMAP_BTN_HEIGHT * 2 + BITMAP_BTN_GAP * 3;
      
      // Position bitmap UI so buttons stay above bottom bar
      bitmapY = max(topBar + BITMAP_MARGIN, screen.height - bottomBar - totalBitmapUIHeight);
      bitmapBtnW = bitmapSize;
      bitmapBtnY = bitmapY + bitmapSize + BITMAP_BTN_GAP;
    }
  }
  
  // Calculate strip button dimensions
  const stripAreaW = max(MIN_STRIP_WIDTH, screen.width - bitmapColumnW);
  const stripAreaH = availableHeight;
  // Ensure strips fit within available height (even with MIN_STRIP_HEIGHT, may need to shrink)
  const idealStripH = floor(stripAreaH / patCount);
  const stripH = max(MIN_STRIP_HEIGHT, min(idealStripH, floor(stripAreaH / patCount)));
  const stripX = 0;
  const stripY = topBar;
  
  // Record button in bottom-left
  const recordBtnW = isCompact ? 52 : 68;
  const recordBtnH = isCompact ? 24 : 32;
  const recordBtnX = BITMAP_MARGIN;
  const recordBtnY = screen.height - recordBtnH - floor((bottomBar - recordBtnH) / 2);
  
  // Pats button in top-right corner
  const patsBtnW = 24;
  const patsBtnH = topBar - 2;
  const patsBtnX = screen.width - patsBtnW - 2;
  const patsBtnY = 1;
  
  // Notepat button next to pats
  const notepatBtnW = 36;
  const notepatBtnH = patsBtnH;
  const notepatBtnX = patsBtnX - notepatBtnW - 6;
  const notepatBtnY = 1;
  
  return {
    mode: 'normal',
    isCompact,
    isNarrow,
    isLandscape,
    bitmapBeside,
    
    // Zones
    topBar,
    bottomBar,
    availableHeight,
    
    // Strip buttons
    stripX,
    stripY,
    stripW: stripAreaW,
    stripH,
    stripAreaH,
    
    // Bitmap preview
    hasBitmap,
    bitmapSize,
    bitmapColumnW,
    bitmapX,
    bitmapY,
    bitmapBtnW,
    bitmapBtnY,
    
    // Control buttons
    recordBtnX,
    recordBtnY,
    recordBtnW,
    recordBtnH,
    patsBtnX,
    patsBtnY,
    patsBtnW,
    patsBtnH,
    notepatBtnX,
    notepatBtnY,
    notepatBtnW,
    notepatBtnH,
  };
}

/**
 * Get cached layout metrics to avoid recalculation every frame.
 */
function getCachedLayout(screen, options) {
  const key = [
    screen.width,
    screen.height,
    options.hasBitmap ? 1 : 0,
    options.isRecording ? 1 : 0,
    options.patCount || 1,
  ].join('|');
  
  if (layoutCache.key === key && layoutCache.metrics) {
    return layoutCache.metrics;
  }
  
  const metrics = getLayoutMetrics(screen, options);
  layoutCache = { key, metrics };
  return metrics;
}

// System
let sfx,
  btns = [],
  pats = 1,
  anyDown = false,
  sampleId,
  sampleData,
  mic,
  micRecordButton,
  notepatButton,
  micRecordButtonLabel = "Connect",
  micConnected = false,
  patsButton,
  bitmapLoopButton,
  bitmapPaintButton,
  bitmapPreview;

let bitmapMeta = null;
let bitmapLooping = false;
let bitmapLoopSound = null;
let bitmapPlaySound = null;
const bitmapSampleId = "stample:bitmap";
let paintJumpPending = false;
let bitmapLoading = false;
let bitmapLoaded = false;
let bitmapProgress = 0; // Playback progress 0-1 for scrubber
let bitmapPlaybackHz = 0; // Current playback rate in Hz (samples per second / total samples)
let lastBitmapProgress = 0; // Previous progress for Hz calculation
let lastProgressTime = 0; // Time of last progress update

// 🎭 KidLisp embedding state - for `stample $code` feature
let kidlispSource = null;      // The KidLisp source code to render
let kidlispCacheId = null;     // The $code identifier (without $)
let kidlispBuffer = null;      // The rendered KidLisp pixel buffer (persistent for layering)
let kidlispLoading = false;    // Whether we're loading KidLisp source
let kidlispActive = false;     // Whether we're in KidLisp sampling mode

const sounds = [],
  progressions = [];

const keyToSfx = { 1: 0, 2: 1, 3: 2, 4: 3, 5: 4, 6: 5, 7: 6, 8: 7, 9: 8, 0: 9 };
const sfxToKey = Object.fromEntries(
  Object.entries(keyToSfx).map(([key, index]) => [index, Number(key)]),
);

async function boot({
  net: { preload },
  sound: { microphone, getSampleData, enabled, registerSample, sampleRate },
  play,
  get,
  ui,
  params,
  screen,
  delay,
  store,
  system,
}) {
  // 🔄 Reset all module state for re-entrancy (when coming back to stample)
  bitmapPreview = null;
  bitmapMeta = null;
  bitmapLooping = false;
  bitmapLoopSound = null;
  bitmapPlaySound = null;
  paintJumpPending = false;
  bitmapLoading = false;
  bitmapLoaded = false;
  bitmapProgress = 0;
  bitmapPlaybackHz = 0;
  lastBitmapProgress = 0;
  lastProgressTime = 0;
  kidlispSource = null;
  kidlispCacheId = null;
  kidlispBuffer = null;
  kidlispLoading = false;
  kidlispActive = false;
  sampleId = undefined;
  sampleData = undefined;
  sounds.length = 0;
  progressions.length = 0;
  layoutCache = { key: null, metrics: null };
  
  // const name = params[0] || "startup";
  const name = "startup"; // TODO: Recall previous samples from `store`.
  if (params[0]) {
    const rawParam = params[0];
    const decodedParam = rawParam.startsWith("%23")
      ? `#${rawParam.slice(3)}`
      : decodeURIComponent(rawParam);
    
    // 🎭 Check for $code KidLisp embedding (e.g., `stample $berz`)
    if (decodedParam.startsWith("$") && decodedParam.length > 1) {
      kidlispCacheId = decodedParam.slice(1); // Remove the $
      kidlispLoading = true;
      kidlispActive = true;
      bitmapLoaded = true; // Mark as loaded so we don't try to load default sample
      sampleId = bitmapSampleId; // Use the bitmap sample ID for KidLisp audio
      console.log(`🎭 Stample: Loading KidLisp piece $${kidlispCacheId}`);
      // KidLisp source will be rendered in sim() each frame
    } else if (decodedParam.startsWith("#")) {
      bitmapLoading = true;
      bitmapLoaded = false;
      bitmapPreview = null;
      bitmapMeta = null;
      const result = await loadPaintingAsAudio(decodedParam, {
        sampleId: bitmapSampleId,
        preload,
        store,
        get,
        sound: { registerSample, sampleRate },
      });
      if (result?.bitmap?.pixels?.length) {
        bitmapPreview = result.bitmap;
        bitmapMeta = result.meta;
        sampleData = result.sampleData;
        sampleId = result.sampleId;
        bitmapLoaded = true;
      }
      bitmapLoading = false;
    } else if (decodedParam === "painting" || decodedParam === "p") {
      bitmapLoading = true;
      bitmapLoaded = false;
      bitmapPreview = null;
      bitmapMeta = null;
      const result = await loadPaintingAsAudio(decodedParam, {
        sampleId: bitmapSampleId,
        system,
        store,
        sound: { registerSample, sampleRate },
      });
      if (result?.bitmap?.pixels?.length) {
        bitmapPreview = result.bitmap;
        bitmapMeta = result.meta;
        sampleData = result.sampleData;
        sampleId = result.sampleId;
        bitmapLoaded = true;
      }
      bitmapLoading = false;
    } else {
      const parsedPats = parseInt(decodedParam);
      if (!Number.isNaN(parsedPats)) pats = parsedPats;
    }
  }
  
  // Only load default sample if we didn't load from a #code bitmap or KidLisp
  if (!bitmapLoaded && !kidlispActive) {
    sampleId = await preload(name);
  }
  
  // Initialize all buttons using layout metrics
  const hasBitmap = !!(bitmapPreview?.pixels?.length);
  const layout = getCachedLayout(screen, { hasBitmap, patCount: pats });
  
  genPats({ screen, ui });
  
  // Record button (bottom left)
  micRecordButton = new ui.Button(
    layout.recordBtnX, 
    layout.recordBtnY, 
    layout.recordBtnW, 
    layout.recordBtnH
  );
  mic = microphone; // Microphone access.

  // Pats button (top right corner)
  patsButton = new ui.Button(
    layout.patsBtnX, 
    layout.patsBtnY, 
    layout.patsBtnW, 
    layout.patsBtnH
  );
  
  // Notepat button (next to pats)
  notepatButton = new ui.Button(
    layout.notepatBtnX,
    layout.notepatBtnY,
    layout.notepatBtnW,
    layout.notepatBtnH,
  );

  // Bitmap control buttons (will be positioned by layoutBitmapUI)
  bitmapLoopButton = new ui.Button(0, 0, BITMAP_MIN_SIZE, BITMAP_BTN_HEIGHT);
  bitmapPaintButton = new ui.Button(0, 0, BITMAP_MIN_SIZE, BITMAP_BTN_HEIGHT);
  layoutBitmapUI(screen);

  if (mic.permission === "granted" && enabled()) {
    // TODO: Also check to see if we have a working audioContext yet here...
    micRecordButtonLabel = BUTTON_LABEL_CONNECTING;
    delay(() => {
      microphone.connect();
    }, 15);
  }

  if (store?.retrieve) {
    // Only restore cached sample if we didn't load from a #code
    if (!bitmapLoaded) {
      const storedSample =
        store["stample:sample"] ||
        (await store.retrieve("stample:sample", "local:db"));
      if (storedSample?.data?.length) {
        const storedId = storedSample.id || "stample";
        sampleId = storedId;
        sampleData = storedSample.data;
        if (registerSample) {
          registerSample(storedId, storedSample.data, storedSample.sampleRate);
        }
      }

      const storedBitmap =
        store["stample:bitmap"] ||
        (await store.retrieve("stample:bitmap", "local:db"));
      if (storedBitmap?.pixels?.length && storedBitmap?.width && storedBitmap?.height) {
        bitmapPreview = {
          width: storedBitmap.width,
          height: storedBitmap.height,
          pixels: new Uint8ClampedArray(storedBitmap.pixels),
        };
        bitmapMeta = {
          sampleLength: storedBitmap.sampleLength,
          sampleRate: storedBitmap.sampleRate,
        };
      }
    }
  }

  // Only fetch sample data if we have a valid sampleId
  if (sampleId) {
    getSampleData(sampleId).then((data) => {
      if (bitmapLoaded) return;
      sampleData = data;
      // console.log("🔴 Sample Data:", sampleData);
    }).catch((err) => {
      console.warn("🔴 Failed to get sample data:", err);
    });
  }
}

function sim({ sound, api, screen, kidlisp, painting }) {
  sounds.forEach((snd, index) => {
    // Get progress data.
    snd?.progress().then((p) => (progressions[index] = p.progress));
  });

  // Track bitmap playback progress for scrubber from any active sound source
  const activeSound = bitmapLoopSound || bitmapPlaySound || sounds.find(s => s);
  if (activeSound) {
    activeSound?.progress?.().then((p) => {
      const newProgress = p?.progress || 0;
      const now = performance.now();
      const deltaTime = now - lastProgressTime;
      
      // Calculate Hz (full cycles per second based on progress change)
      if (deltaTime > 0 && lastProgressTime > 0) {
        const deltaProgress = Math.abs(newProgress - lastBitmapProgress);
        // Hz = (progress change per ms) * 1000 ms/s * (1 / 1 full cycle)
        bitmapPlaybackHz = (deltaProgress / deltaTime) * 1000;
      }
      
      lastBitmapProgress = newProgress;
      lastProgressTime = now;
      bitmapProgress = newProgress;
    });
  } else {
    bitmapProgress = 0;
    bitmapPlaybackHz = 0;
  }

  mic?.poll(); // Query for updated amplitude and waveform data.
  sound.speaker?.poll();
  
  // Live encode recording buffer to bitmap preview
  if (mic?.recording && mic?.recordingBuffer?.buffer?.length > 0) {
    const liveEncoded = encodeSampleToBitmap(mic.recordingBuffer.buffer);
    if (liveEncoded) {
      bitmapPreview = {
        width: liveEncoded.width,
        height: liveEncoded.height,
        pixels: liveEncoded.pixels,
      };
    }
  }
  
  // 🎭 KidLisp rendering: Update the buffer each frame when in KidLisp mode
  if (kidlispActive && kidlispCacheId && painting && screen) {
    // Determine buffer size - use a reasonable default or match screen aspect
    const bufferSize = 128; // Square buffer for consistent sampling
    const bufferWidth = bufferSize;
    const bufferHeight = bufferSize;
    
    // Use the painting() function from sim to create a proper buffer context
    // This ensures $activePaintApi is set correctly when kidlisp() is called
    try {
      const lispPainting = painting(bufferWidth, bufferHeight, (paintApi) => {
        // Paste previous buffer first for accumulation/layering
        if (kidlispBuffer?.pixels?.length) {
          paintApi.paste(kidlispBuffer, 0, 0);
        }
        // Now call kidlisp on this buffer's paintApi - this sets $activePaintApi correctly
        paintApi.kidlisp(0, 0, bufferWidth, bufferHeight, `$${kidlispCacheId}`);
      });
      
      // Debug: Log what we got back
      if (kidlispLoading) {
        if (!lispPainting) {
          console.log(`🎭 Stample: painting() returned null for $${kidlispCacheId}`);
        } else if (!lispPainting.pixels) {
          console.log(`🎭 Stample: painting() returned buffer with no pixels for $${kidlispCacheId}`, Object.keys(lispPainting));
        } else if (!lispPainting.pixels.length) {
          console.log(`🎭 Stample: painting() returned buffer with empty pixels for $${kidlispCacheId}`);
        } else {
          console.log(`🎭 Stample: painting() returned valid buffer ${lispPainting.width}x${lispPainting.height} with ${lispPainting.pixels.length} pixels`);
        }
      }
      
      // Extract pixels from the returned painting buffer
      if (lispPainting?.pixels?.length) {
        kidlispBuffer = {
          width: bufferWidth,
          height: bufferHeight,
          pixels: new Uint8ClampedArray(lispPainting.pixels),
        };
        
        // Update the bitmap preview with the KidLisp render
        bitmapPreview = kidlispBuffer;
        
        // Convert to audio sample
        const totalPixels = bufferWidth * bufferHeight;
        bitmapMeta = {
          sampleLength: totalPixels * 3, // RGB = 3 samples per pixel
          sampleRate: sound?.sampleRate || 48000,
        };
        
        // Decode pixels to audio sample and register it EVERY frame for live updates
        const decoded = decodeBitmapToSample(kidlispBuffer, bitmapMeta);
        if (decoded?.length) {
          sampleData = decoded;
          sampleId = bitmapSampleId;
          
          // 🔴 LIVE AUDIO UPDATE: Use updateSample for truly seamless buffer swapping
          // Check if any sounds are currently playing that use this sample
          const hasPlayingSounds = bitmapLoopSound || sounds.some(s => s);
          
          if (hasPlayingSounds && sound?.updateSample) {
            // Update the buffer in place - no crossfade needed, maintains position
            console.log(`🔴 LIVE BUFFER UPDATE: Sending updateSample for ${bitmapSampleId}`);
            sound.updateSample(bitmapSampleId, decoded, bitmapMeta.sampleRate);
          } else {
            // Re-register sample for NEW sounds to use the latest buffer
            if (sound?.registerSample) {
              sound.registerSample(bitmapSampleId, decoded, bitmapMeta.sampleRate);
            }
          }
          
          if (kidlispLoading) {
            kidlispLoading = false;
            console.log(`🎭 Stample: KidLisp $${kidlispCacheId} live audio registered (${decoded.length} samples at ${bitmapMeta.sampleRate}Hz)`);
          }
        }
      }
    } catch (err) {
      if (kidlispLoading) {
        console.warn(`🎭 Stample: Error rendering KidLisp $${kidlispCacheId}:`, err);
      }
    }
  }
}

function paint({ api, wipe, ink, sound, screen, num, text, help, pens }) {
  const isRecording = mic?.recording;
  const hasBitmap = !!(bitmapPreview?.pixels?.length);
  const layout = getCachedLayout(screen, { hasBitmap, isRecording, patCount: pats });
  
  // ─────────────────────────────────────────────────────────────────────────
  // 🔴 RECORDING MODE
  // ─────────────────────────────────────────────────────────────────────────
  if (isRecording) {
    wipe(40, 0, 0); // Dark red background
    
    // Dark background for preview area
    ink("black", 220).box(
      layout.previewX - 4, 
      layout.previewY - 4, 
      layout.previewW + 8, 
      layout.previewH + 8
    );
    
    if (bitmapPreview?.pixels?.length) {
      // Draw the live-filling bitmap
      api.paste(bitmapPreview, layout.previewX, layout.previewY, {
        width: layout.previewW,
        height: layout.previewH,
      });
    } else {
      ink("red", 60).box(layout.previewX, layout.previewY, layout.previewW, layout.previewH);
      ink("white", 120).write(
        "recording...", 
        layout.previewX + layout.previewW / 2 - 30, 
        layout.previewY + layout.previewH / 2 - 6
      );
    }
    
    // Show recording stats at top
    const sampleCount = mic?.recordingBuffer?.length || 0;
    const fullLength = sampleCount * 128; // Undo downsampling to estimate real length
    const duration = fullLength > 0 ? (fullLength / 48000).toFixed(1) : "0.0";
    ink("white").write(`REC ${duration}s`, 8, 4);
    if (bitmapPreview?.width) {
      const sizeText = `${bitmapPreview.width}x${bitmapPreview.height}`;
      ink("yellow").write(sizeText, screen.width - text.width(sizeText) - 8, 4);
    }
    
    // Live waveform indicator
    if (mic?.waveform?.length > 0 && !layout.isCompact) {
      const waveW = 50;
      const waveH = 20;
      const waveX = screen.width - waveW - 8;
      const waveY = layout.topBar + 4;
      ink("black", 150).box(waveX - 2, waveY - 2, waveW + 4, waveH + 4);
      sound.paint.waveform(api, mic.amplitude, mic.waveform, waveX, waveY, waveW, waveH);
    }
    
    // Draw STOP button
    ink("white").box(layout.stopBtnX, layout.stopBtnY, layout.stopBtnW, layout.stopBtnH);
    ink("red").box(layout.stopBtnX + 3, layout.stopBtnY + 3, layout.stopBtnW - 6, layout.stopBtnH - 6);
    const stopText = "STOP";
    ink("white").write(
      stopText, 
      layout.stopBtnX + layout.stopBtnW / 2 - text.width(stopText) / 2, 
      layout.stopBtnY + layout.stopBtnH / 2 - 6
    );
    
    // Update record button box for hit testing
    micRecordButton.box.x = layout.stopBtnX;
    micRecordButton.box.y = layout.stopBtnY;
    micRecordButton.box.w = layout.stopBtnW;
    micRecordButton.box.h = layout.stopBtnH;
    
    return; // Skip normal UI during recording
  }
  
  // ─────────────────────────────────────────────────────────────────────────
  // 🎹 NORMAL MODE
  // ─────────────────────────────────────────────────────────────────────────
  wipe(0, 0, 255);
  
  btns.forEach((btn, index) => {
    btn.paint(() => {
      ink(btn.down ? "white" : "cyan").box(btn.box); // Paint box a teal color.
      ink("black").box(btn.box, "out"); // Outline in black.

      let options = sounds[index]?.options;

      // Debug: Log needle state periodically
      if (index === 0 && Math.random() < 0.02) {
        console.log(`🟠 NEEDLE[${index}]: prog=${progressions[index]}, options=${JSON.stringify(options)}, sound=${!!sounds[index]}`);
      }

      // Render playback needle within this button's box (old working logic)
      if (options && progressions[index] !== undefined) {
        let y;
        const progress = progressions[index];
        const speed = options.speed ?? 1;
        const to = options.to ?? 1;
        // Check direction based on speed
        if (speed > 0 || !speed) {
          // Forward playback: needle moves from bottom to top of button
          y = btn.box.y + (1 - progress) * btn.box.h;
        } else {
          // Reverse playback: adjust for backwards movement
          y = btn.box.y + (1 - to * progress) * btn.box.h;
        }
        // Keep the needle inside the visible box
        const minY = btn.box.y + 1;
        const maxY = btn.box.y + btn.box.h - 2;
        y = Math.max(minY, Math.min(maxY, y));
        ink("orange").line(0, y, btn.box.x + btn.box.w, y);
      }

      ink("black").write(
        sfxToKey[btns.length - 1 - index],
        btn.box.x + 4,
        btn.box.y + 4,
      );
    });

    // Only show record button if NOT in KidLisp mode
    if (!kidlispActive) {
      micRecordButton.paint((btn) => {
        const color = mic.connected ? "red" : "orange";
        //if (mic.connected)

        ink(btn.down ? "white" : color).box(btn.box);
        ink(btn.down ? color : "white").box(btn.box, "inline");

        ink(btn.down ? color : "white").write(
          micRecordButtonLabel,
          btn.box.x + btn.box.w / 2 - text.width(micRecordButtonLabel) / 2,
          btn.box.y + btn.box.h / 2 - text.height(micRecordButtonLabel) / 2,
        );

        // Graph microphone (1 channel)
        if (mic?.waveform.length > 0 && mic?.amplitude !== undefined) {
          sound.paint.waveform(
            api,
            mic.amplitude,
            mic.waveform,
            btn.box.x,
            btn.box.y,
            btn.box.w - 1,
            btn.box.h,
          );
        }
      });
    }
  });

  patsButton.paint((btn) => {
    ink("yellow", btn.down ? 128 : 64).box(btn.box);
  });

  notepatButton.paint((btn) => {
    ink("cyan", btn.down ? 160 : 90).box(btn.box);
    ink("black").box(btn.box, "out");
    ink("black").write("pat", btn.box.x + 6, btn.box.y + 6);
  });

  // Only show loop button if NOT in KidLisp mode AND there's a bitmap
  if (!kidlispActive && bitmapPreview?.pixels?.length) {
    bitmapLoopButton?.paint((btn) => {
      const label = bitmapLooping ? "Stop" : "Loop";
      const bg = bitmapLooping ? "magenta" : "purple";
      ink(bg, btn.down ? 200 : 120).box(btn.box);
      ink("black").box(btn.box, "out");
      ink("white").write(
        label,
        btn.box.x + btn.box.w / 2 - text.width(label) / 2,
        btn.box.y + btn.box.h / 2 - text.height(label) / 2,
      );
    });
  }

  // Only show paint button if NOT in KidLisp mode AND there's a bitmap
  if (!kidlispActive && bitmapPreview?.pixels?.length) {
    bitmapPaintButton?.paint((btn) => {
      const label = "Paint";
      const bg = "lime";
      ink(bg, btn.down ? 200 : 120).box(btn.box);
      ink("black").box(btn.box, "out");
      ink("black").write(
        label,
        btn.box.x + btn.box.w / 2 - text.width(label) / 2,
        btn.box.y + btn.box.h / 2 - text.height(label) / 2,
      );
    });
  }

  ink("white").write(pats, { right: pats > 9 ? 6 : 8, top: 6 });

  // console.log(sound.speaker.amplitudes.left);

  // Audio level bars (top area)
  const barsX = 54;
  const barsW = layout.notepatBtnX - barsX - 8;
  sound.paint.bars(
    api,
    sound.speaker.amplitudes.left,
    help.resampleArray(sound.speaker.waveforms.left, 16),
    barsX,
    0,
    barsW,
    layout.topBar - 2,
    [255, 0, 0, 255],
  );

  // Use bitmap sample data for waveform if loaded, otherwise use regular sample
  const waveformData = bitmapLoaded && sampleData ? sampleData : sampleData;
  const waveformColor = bitmapLoaded ? [255, 100, 0, 48] : [0, 0, 255, 32]; // Orange for bitmap, blue for regular
  
  // Background waveform (in strip button area)
  if (waveformData) {
    const waveX = layout.stripX;
    const waveY = layout.stripY;
    const waveW = layout.stripW;
    const waveH = layout.stripAreaH;
    sound.paint.waveform(
      api,
      num.arrMax(waveformData),
      num.arrCompress(waveformData, 256), // 🔴 TODO: This could be made much faster.
      waveX,
      waveY,
      waveW,
      waveH,
      waveformColor,
      { direction: "bottom-to-top" },
    );
  }

  // Bitmap preview (if present) - uses actual aspect ratio
  if (layout.hasBitmap && bitmapPreview?.pixels?.length) {
    const bmpAspect = bitmapPreview.width / bitmapPreview.height;
    let previewW, previewH;
    if (bmpAspect >= 1) {
      // Wider than tall
      previewW = layout.bitmapSize;
      previewH = floor(layout.bitmapSize / bmpAspect);
    } else {
      // Taller than wide
      previewH = layout.bitmapSize;
      previewW = floor(layout.bitmapSize * bmpAspect);
    }
    const previewX = layout.bitmapX + floor((layout.bitmapSize - previewW) / 2);
    const previewY = layout.bitmapY + floor((layout.bitmapSize - previewH) / 2);

    ink("black", 160).box(previewX - 2, previewY - 2, previewW + 4, previewH + 4);
    api.paste(bitmapPreview, previewX, previewY, {
      width: previewW,
      height: previewH,
    });
    
    // Draw scrubber line showing playback progress
    if (bitmapProgress > 0) {
      const totalPixels = bitmapPreview.width * bitmapPreview.height;
      const currentPixel = Math.floor(bitmapProgress * totalPixels);
      const scrubY = Math.floor(currentPixel / bitmapPreview.width);
      const scrubX = currentPixel % bitmapPreview.width;
      // Map to preview coordinates
      const mappedY = previewY + (scrubY / bitmapPreview.height) * previewH;
      const mappedX = previewX + (scrubX / bitmapPreview.width) * previewW;
      // Draw horizontal line at current row
      ink("yellow", 200).line(previewX, mappedY, previewX + previewW, mappedY);
      // Draw small marker at exact position
      ink("red").box(mappedX - 1, mappedY - 1, 3, 3);
      
      // 📊 Hz readout - show playback rate below the preview
      if (bitmapPlaybackHz > 0.001) {
        const hzText = bitmapPlaybackHz >= 1 
          ? `${bitmapPlaybackHz.toFixed(1)} Hz`
          : `${(bitmapPlaybackHz * 1000).toFixed(0)} mHz`;
        ink("cyan", 200).write(hzText, previewX, previewY + previewH + 4);
      }
    }
  } else if (bitmapLoading) {
    const previewX = layout.bitmapX || (screen.width - BITMAP_MIN_SIZE - BITMAP_MARGIN);
    const previewY = layout.bitmapY || layout.topBar + BITMAP_MARGIN;
    const previewW = layout.bitmapSize || BITMAP_MIN_SIZE;
    const previewH = layout.bitmapSize || BITMAP_MIN_SIZE;
    ink("black", 120).box(previewX - 2, previewY - 2, previewW + 4, previewH + 4);
    ink("white").write("loading", previewX + 6, previewY + previewH / 2 - 6);
  }

  if (pens()) {
    pens().forEach((p) => {
      if (p.dragBox) {
        ink().line(
          p.dragBox.x,
          p.dragBox.y,
          p.dragBox.x + (p.dragBox.w || 0),
          p.dragBox.y + (p.dragBox.h || 0),
        );
      }
    });
  }
}

const btnSounds = {};

function act({ event: e, sound, pens, screen, ui, notice, beep, store, jump, system, needsPaint }) {
  const sliceLength = 1 / btns.length; // Divide the total duration (1.0) by the number of buttons.

  // 🎭 KidLisp mode: clicking on bitmap toggles looping for live buffer updates
  if (kidlispActive) {
    btns.forEach((btn) => {
      btn.act(e, {
        down: () => {
          if (!kidlispBuffer?.pixels?.length) return;
          
          // Toggle looping
          if (bitmapLooping) {
            // Stop looping
            bitmapLoopSound?.kill?.(0.1);
            bitmapLoopSound = null;
            bitmapLooping = false;
            bitmapProgress = 0;
          } else {
            // Start looping
            const decoded = decodeBitmapToSample(kidlispBuffer, bitmapMeta);
            if (!decoded?.length) return;
            sound.registerSample?.(bitmapSampleId, decoded, bitmapMeta?.sampleRate || sound.sampleRate);
            bitmapPlaySound?.kill?.(0.05);
            bitmapPlaySound = null;
            bitmapProgress = 0;
            bitmapLoopSound = sound.play(bitmapSampleId, { loop: true });
            bitmapLooping = true;
            console.log(`🎭 KidLisp: Started loop for live buffer updates`);
          }
        },
      });
    });
    return; // Skip normal button handling in KidLisp mode
  }

  btns.forEach((btn, index) => {
    let from = (btns.length - 1 - index) * sliceLength;
    let to = from + sliceLength;
    btn.act(
      e,
      {
        down: (btn, opts) => {
          // if (downs[note]) return false; // Cancel the down if the key is held.
          anyDown = true;
          if (btn.down) return false;

          // if (opts?.keyboard) {
          // const fromPos = from; // 1 - (/*e.y -*/ 0 -  btn.box.y) / btn.box.h;
          // from = fromPos;
          sounds[index] = sound.play(sampleId, { from, to, loop: true });
          // }

          // const fromPos = 1 - (e.y - btn.box.y) / btn.box.h;
          // from = fromPos;
          // sounds[index] = sound.play(sampleId, { from, to, loop });

          // TODO: Figure out a cool attack and decay on these.
          // Only disconnect mic if actively recording to avoid destroying
          // the mic connection on every strip press (which forced the user
          // to reconnect before they could record again).
          if (sound.microphone.recording) {
            sound.microphone.cut().catch(() => {}); // Discard the partial recording.
            micRecordButtonLabel = "Record";
          }
        },

        over: (btn) => {
          // if (btn.up && anyDown) {
          //  btn.up = false;
          //  btn.actions.down(btn);
          // }
          // console.log("over");
        },
        out: (btn) => {
          // btn.down = false;
          // btn.actions.up(btn);
        },
        up: (btn, opts) => {
          // Always kill the sound and release the button to prevent stuck states.
          // Previously, returning false on pointer mismatch left btn.down=true permanently.
          sounds[index]?.kill(0.1);
          sounds[index] = undefined; // Clear dead ref so sim() doesn't poll it.
          delete btnSounds[index];
          return true;
        },
        cancel: (btn) => {
          // Kill the sound when button is cancelled (e.g., mouse leaves screen)
          sounds[index]?.kill(0.1);
          sounds[index] = undefined;
          delete btnSounds[index];
          anyDown = false;
        },
        scrub: (btn) => {
          /*
          if (abs(e.delta.y) > 0 && !btnSounds[index]) {
            // console.log(`Scrub ${index}:`, e.delta);
            // sounds[index] = sound.play(sampleId, { from, to });
            const fromPos = 1 - (e.y - btn.box.y) / btn.box.h;
            from = fromPos;

            let speed = 1;

            if (e.delta.y > 0) {
              // to = 0;
              let tmpFrom = from;
              from = 0;
              to = tmpFrom;
              speed = -1;
            }

            // TODO: How do all these parameters relate?
            // console.log("📗 From:", from, "To:", to);

            btnSounds[index] = true;
            sounds[index] = sound.play(sampleId, { from, to, speed, loop });
          }
          */

          // if (e.pointer === btn.downPointer) {
          if (abs(e.delta.y) > 0) {
            // Simple scrub: shift speed based on drag delta (old working logic)
            const shiftAmount = 0.03 * -e.delta.y;
            const snd = sounds[index];
            
            console.log(`🎚️ SCRUB[${index}]: delta.y=${e.delta.y.toFixed(2)}, shift=${shiftAmount.toFixed(4)}, sound=${!!snd}, options=${JSON.stringify(snd?.options)}`);
            
            if (snd) {
              snd.update({ shift: shiftAmount });
              console.log(`🎚️ SCRUB[${index}]: sent shift update`);
            } else {
              console.log(`🎚️ SCRUB[${index}]: NO SOUND to update!`);
            }
          }
          // }
        },
      },
      pens?.(),
    );
  });

  micRecordButton?.act(e, {
    down: () => {
      if (!sound.microphone.connected) {
        sound.microphone.connect();
        micRecordButtonLabel = BUTTON_LABEL_CONNECTING;
      } else {
        setTimeout(() => {
          sound.microphone.rec(); // Start recording.
        }, 50);
      }
    },
    up: async (btn) => {
      if (sound.microphone.recording) {
        const { id, data } = await sound.microphone.cut(); // Get the sample.
        const storedId = "stample";
        sampleData = data;
        sampleId = storedId;
        sound.registerSample?.(storedId, data, sound.sampleRate);

        const encoded = encodeSampleToBitmap(data);
        if (encoded) {
          bitmapPreview = {
            width: encoded.width,
            height: encoded.height,
            pixels: encoded.pixels,
          };
          bitmapMeta = {
            sampleLength: encoded.sampleLength,
            sampleRate: sound.sampleRate,
          };
          if (store) {
            store["stample:bitmap"] = {
              width: encoded.width,
              height: encoded.height,
              pixels: Array.from(encoded.pixels),
              sampleLength: encoded.sampleLength,
              sampleRate: sound.sampleRate,
            };
            store.persist?.("stample:bitmap", "local:db");
          }
        }

        if (store) {
          store["stample:sample"] = {
            id: storedId,
            data,
            sampleRate: sound.sampleRate,
          };
          store.persist?.("stample:sample", "local:db");
        }
        console.log("🎤 Microphone sample id:", sampleId);
      }
    },
  });

  bitmapLoopButton?.act(e, {
    up: () => {
      if (!bitmapPreview?.pixels?.length) return;

      if (bitmapLooping) {
        bitmapLoopSound?.kill?.(0.1);
        bitmapLoopSound = null;
        bitmapLooping = false;
        bitmapProgress = 0; // Reset scrubber
        return;
      }

      const decoded = decodeBitmapToSample(bitmapPreview, bitmapMeta);
      if (!decoded?.length) return;

      sound.registerSample?.(
        bitmapSampleId,
        decoded,
        bitmapMeta?.sampleRate || sound.sampleRate,
      );
      bitmapPlaySound?.kill?.(0.05);
      bitmapPlaySound = null;
      bitmapProgress = 0;
      bitmapLoopSound = sound.play(bitmapSampleId, { loop: true });
      bitmapLooping = true;
    },
  });

  bitmapPaintButton?.act(e, {
    up: () => {
      if (!bitmapPreview?.pixels?.length || !system?.nopaint?.replace) return;
      if (paintJumpPending) return;
      paintJumpPending = true;
      system.nopaint.replace({ system, store, needsPaint }, bitmapPreview, "stample");
      if (store) {
        store["painting:tags"] = ["stample"];
        store.persist?.("painting:tags", "local:db");
      }
      setTimeout(() => {
        jump?.("prompt");
      }, 30);
      setTimeout(() => {
        paintJumpPending = false;
      }, 1500);
    },
  });

  patsButton?.act(e, () => {
    pats += 1;
    beep();
    if (pats > maxPats) pats = 1;
    genPats({ screen, ui });
  });

  notepatButton?.act(e, () => {
    jump?.("notepat:stample");
  });

  if (e.is("microphone-connect:failure")) {
    console.log("🎤 Failed mic connection:", e);
    if (e.reason) notice(e.reason.toUpperCase(), ["yellow", "red"]);
    micRecordButtonLabel = "Connect";
  }

  if (e.is("microphone-connect:success")) {
    console.log("🎤 Connected.");
    micRecordButtonLabel = "Record";
  }

  if (e.is("microphone-disconnect")) {
    micRecordButtonLabel = "Connect";
  }

  if (e.is("keyboard:down") || e.is("keyboard:up")) {
    const index = keyToSfx[e.key];
    if (index !== undefined && btns[btns.length - 1 - index]) {
      // Ensure index is valid and button exists
      const btn = btns[btns.length - 1 - index];
      if (e.is("keyboard:down") && !btn.down) {
        // console.log(`${e.key} key pressed!`);
        btn.actions.down(btn, { keyboard: true });
        btn.downPointer = 0;
        btn.down = true;
      } else if (e.is("keyboard:up")) {
        btn.down = false;
        btn.actions.up(btn, { keyboard: true });
        btn.downPointer = undefined;
      }
    }
  }

  if (e.is("keyboard:down:arrowdown")) {
    pats -= 1;
    if (pats < 1) pats = maxPats;
    beep();
    genPats({ screen, ui });
  }

  if (e.is("keyboard:down:arrowup")) {
    pats += 1;
    if (pats > maxPats) pats = 1;
    beep();
    genPats({ screen, ui });
  }

  if (e.is("reframed")) {
    // Regenerate strip buttons and all UI with new layout
    genPats({ screen, ui });
    
    // Get layout metrics for control button repositioning
    const hasBitmap = !!(bitmapPreview?.pixels?.length);
    const layout = getCachedLayout(screen, { hasBitmap, patCount: pats });
    
    // Reposition record button using layout metrics
    micRecordButton.box.x = layout.recordBtnX;
    micRecordButton.box.y = layout.recordBtnY;
    micRecordButton.box.w = layout.recordBtnW;
    micRecordButton.box.h = layout.recordBtnH;
    
    // Reposition pats button
    patsButton.box.x = layout.patsBtnX;
    patsButton.box.y = layout.patsBtnY;
    patsButton.box.w = layout.patsBtnW;
    patsButton.box.h = layout.patsBtnH;
    
    // Reposition notepat button
    notepatButton.box.x = layout.notepatBtnX;
    notepatButton.box.y = layout.notepatBtnY;
    notepatButton.box.w = layout.notepatBtnW;
    notepatButton.box.h = layout.notepatBtnH;
    
    // Reposition bitmap UI elements
    layoutBitmapUI(screen);
  }
}

function leave({ sound }) {
  // Clean up all playing sounds to avoid orphaned audio on piece transitions.
  sounds.forEach((snd, i) => {
    snd?.kill?.(0.05);
    sounds[i] = undefined;
  });
  sounds.length = 0;
  bitmapLoopSound?.kill?.(0.05);
  bitmapLoopSound = null;
  bitmapPlaySound?.kill?.(0.05);
  bitmapPlaySound = null;
  bitmapLooping = false;
}

export { boot, paint, act, sim, leave };

// 📚 Library

// Generate sectional strips of buttons to split the sample by.
function genPats({ screen, ui }) {
  const hasBitmap = !!(bitmapPreview?.pixels?.length);
  const layout = getCachedLayout(screen, { hasBitmap, patCount: pats });
  
  btns.length = 0;
  for (let i = 0; i < pats; i += 1) {
    const x = layout.stripX;
    const y = layout.stripY + layout.stripH * i;
    const width = layout.stripW;
    const height = layout.stripH;
    const button = new ui.Button(x, y, width, height);
    button.stickyScrubbing = true; // Keep scrubbing on the original button, allow off-screen movement
    button.noRolloverActivation = true; // Prevent activating other buttons when dragging from a sticky button
    btns.push(button);
  }
}

function layoutBitmapUI(screen) {
  if (!bitmapLoopButton) return;
  
  const hasBitmap = !!(bitmapPreview?.pixels?.length);
  const layout = getCachedLayout(screen, { hasBitmap, patCount: pats });
  
  // Only position bitmap buttons when there's actually a bitmap to show
  if (hasBitmap && layout.bitmapSize > 0) {
    // Loop button
    bitmapLoopButton.box.w = layout.bitmapBtnW || layout.bitmapSize || 80;
    bitmapLoopButton.box.h = BITMAP_BTN_HEIGHT;
    bitmapLoopButton.box.x = layout.bitmapX;
    bitmapLoopButton.box.y = layout.bitmapBtnY;

    // Paint button (below loop button)
    if (bitmapPaintButton) {
      bitmapPaintButton.box.w = bitmapLoopButton.box.w;
      bitmapPaintButton.box.h = BITMAP_BTN_HEIGHT;
      bitmapPaintButton.box.x = bitmapLoopButton.box.x;
      bitmapPaintButton.box.y = bitmapLoopButton.box.y + BITMAP_BTN_HEIGHT + BITMAP_BTN_GAP;
    }
  } else {
    // Move buttons off-screen when no bitmap
    bitmapLoopButton.box.x = -1000;
    bitmapLoopButton.box.y = -1000;
    if (bitmapPaintButton) {
      bitmapPaintButton.box.x = -1000;
      bitmapPaintButton.box.y = -1000;
    }
  }
}

