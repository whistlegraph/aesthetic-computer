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

const { abs, max, floor } = Math;

// Layout Constants
const maxPats = 10;

// Responsive Layout Thresholds
const COMPACT_HEIGHT = 160;

// Layout Zone Defaults
const TOP_BAR_HEIGHT = 24;
const BOTTOM_BAR_HEIGHT = 36;
const COMPACT_TOP_BAR = 18;
const COMPACT_BOTTOM_BAR = 28;

// Bitmap Control Buttons
const BITMAP_BTN_HEIGHT = 18;
const BITMAP_BTN_GAP = 4;
const BARS_MIN_X = 54;

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
function getLayoutMetrics(screen, { hasBitmap = false, patCount = 1 } = {}) {
  const isCompact = screen.height < COMPACT_HEIGHT;
  
  // Calculate zone heights
  const topBar = isCompact ? COMPACT_TOP_BAR : TOP_BAR_HEIGHT;
  const bottomBar = isCompact ? COMPACT_BOTTOM_BAR : BOTTOM_BAR_HEIGHT;
  const availableHeight = max(0, screen.height - topBar - bottomBar);
  
  // Calculate strip button dimensions
  const stripAreaW = max(MIN_STRIP_WIDTH, screen.width);
  const stripAreaH = availableHeight;
  const safePatCount = max(1, patCount);
  const stripH = max(MIN_STRIP_HEIGHT, floor(stripAreaH / safePatCount));
  const stripX = 0;
  const stripY = topBar;
  
  // Record button is always a full-width bottom strip.
  const recordBtnW = screen.width;
  const recordBtnH = bottomBar;
  const recordBtnX = 0;
  const recordBtnY = screen.height - bottomBar;
  
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

  // Loop/Paint controls live in top bar, never over the board.
  const bitmapLoopBtnW = isCompact ? 34 : 42;
  const bitmapPaintBtnW = isCompact ? 42 : 52;
  const bitmapBtnH = max(12, topBar - 6);
  const bitmapBtnY = floor((topBar - bitmapBtnH) / 2);
  const bitmapLoopBtnX = 2;
  const bitmapPaintBtnX = bitmapLoopBtnX + bitmapLoopBtnW + BITMAP_BTN_GAP;
  const barsX = hasBitmap ? bitmapPaintBtnX + bitmapPaintBtnW + 6 : BARS_MIN_X;
  const barsW = max(0, notepatBtnX - barsX - 8);
  
  return {
    mode: 'normal',
    isCompact,
    
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
    
    // Bitmap controls
    hasBitmap,
    bitmapLoopBtnX,
    bitmapPaintBtnX,
    bitmapBtnY,
    bitmapLoopBtnW,
    bitmapPaintBtnW,
    bitmapBtnH,
    barsX,
    barsW,
    
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
  bitmapLoopButton = new ui.Button(0, 0, 42, BITMAP_BTN_HEIGHT);
  bitmapPaintButton = new ui.Button(0, 0, 52, BITMAP_BTN_HEIGHT);
  layoutBitmapUI(screen);

  if (mic.permission === "granted" && enabled()) {
    // TODO: Also check to see if we have a working audioContext yet here...
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
        layoutBitmapUI(screen);
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
  const hasBitmap = !!(bitmapPreview?.pixels?.length);
  const layout = getCachedLayout(screen, { hasBitmap, patCount: pats });
  
  // ─────────────────────────────────────────────────────────────────────────
  // 🎹 NORMAL MODE
  // ─────────────────────────────────────────────────────────────────────────
  wipe(0, 0, 255);

  // Loading state for bitmap.
  if (!hasBitmap && bitmapLoading) {
    ink("black", 120).box(layout.stripX, layout.stripY, layout.stripW, layout.stripAreaH);
    ink("white", 170).write(
      "loading",
      layout.stripX + layout.stripW / 2 - text.width("loading") / 2,
      layout.stripY + layout.stripAreaH / 2 - text.height("loading") / 2,
    );
  }

  // Use bitmap sample data for waveform if loaded, otherwise use regular sample.
  const waveformData = sampleData;
  const waveformColor = hasBitmap ? [255, 100, 0, 28] : [0, 0, 255, 24];

  // Background waveform (in strip button area)
  if (waveformData) {
    sound.paint.waveform(
      api,
      num.arrMax(waveformData),
      num.arrCompress(waveformData, 256), // 🔴 TODO: This could be made much faster.
      layout.stripX,
      layout.stripY,
      layout.stripW,
      layout.stripAreaH,
      waveformColor,
      { direction: "bottom-to-top" },
    );
  }

  btns.forEach((btn, index) => {
    btn.paint(() => {
      ink(btn.down ? "white" : "cyan", btn.down ? 120 : 130).box(btn.box);
      ink("black").box(btn.box, "out"); // Outline in black.

      ink("black").write(
        sfxToKey[btns.length - 1 - index],
        btn.box.x + 4,
        btn.box.y + 4,
      );
    });
  });

  // Paint bitmap preview ON TOP of buttons so it stays visible while playing.
  if (hasBitmap) {
    api.paste(bitmapPreview, layout.stripX, layout.stripY, {
      width: layout.stripW,
      height: layout.stripAreaH,
    });

    // Draw scrubber line over mapped board.
    if (bitmapProgress > 0) {
      const totalPixels = bitmapPreview.width * bitmapPreview.height;
      const currentPixel = floor(bitmapProgress * totalPixels);
      const scrubY = floor(currentPixel / bitmapPreview.width);
      const mappedY = layout.stripY + (scrubY / bitmapPreview.height) * layout.stripAreaH;
      ink("yellow", 200).line(layout.stripX, mappedY, layout.stripX + layout.stripW, mappedY);
    }
  }

  // Render playback needles on top of everything in the strip area.
  btns.forEach((btn, index) => {
    const options = sounds[index]?.options;
    if (options && progressions[index] !== undefined) {
      let y;
      const progress = progressions[index];
      const speed = options.speed ?? 1;
      const to = options.to ?? 1;
      if (speed > 0 || !speed) {
        y = btn.box.y + (1 - progress) * btn.box.h;
      } else {
        y = btn.box.y + (1 - to * progress) * btn.box.h;
      }
      const minY = btn.box.y + 1;
      const maxY = btn.box.y + btn.box.h - 2;
      y = Math.max(minY, Math.min(maxY, y));
      ink("orange").line(0, y, btn.box.x + btn.box.w, y);
    }
  });

  // Only show record button if NOT in KidLisp mode.
  if (!kidlispActive) {
    micRecordButton.paint((btn) => {
      const label = mic?.connected ? "HOLD TO RECORD" : "CONNECT MICROPHONE";
      const color = mic?.connected ? "red" : "orange";
      ink(color, mic?.recording ? 255 : btn.down ? 220 : 180).box(btn.box);
      ink("white", 190).box(btn.box, "out");

      // Graph microphone (1 channel) behind text for live feedback while holding.
      if (mic?.waveform?.length > 0 && mic?.amplitude !== undefined) {
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

      ink("white").write(
        label,
        btn.box.x + btn.box.w / 2 - text.width(label) / 2,
        btn.box.y + btn.box.h / 2 - text.height(label) / 2,
      );
    });
  }

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
  if (layout.barsW > 0) {
    sound.paint.bars(
      api,
      sound.speaker.amplitudes.left,
      help.resampleArray(sound.speaker.waveforms.left, 16),
      layout.barsX,
      0,
      layout.barsW,
      layout.topBar - 2,
      [255, 0, 0, 255],
    );
  }

  if (bitmapPlaybackHz > 0.001 && hasBitmap) {
    const hzText = bitmapPlaybackHz >= 1
      ? `${bitmapPlaybackHz.toFixed(1)} Hz`
      : `${(bitmapPlaybackHz * 1000).toFixed(0)} mHz`;
    ink("black", 140).box(layout.stripX + 4, layout.stripY + 2, text.width(hzText) + 4, text.height(hzText) + 2);
    ink("cyan", 220).write(hzText, layout.stripX + 6, layout.stripY + 4);
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
          layoutBitmapUI(screen);
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
  }

  if (e.is("microphone-connect:success")) {
    console.log("🎤 Connected.");
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
  
  // Only position bitmap buttons when there's actually a bitmap to show.
  if (hasBitmap) {
    // Loop button
    bitmapLoopButton.box.w = layout.bitmapLoopBtnW;
    bitmapLoopButton.box.h = layout.bitmapBtnH;
    bitmapLoopButton.box.x = layout.bitmapLoopBtnX;
    bitmapLoopButton.box.y = layout.bitmapBtnY;

    // Paint button (to the right of loop button)
    if (bitmapPaintButton) {
      bitmapPaintButton.box.w = layout.bitmapPaintBtnW;
      bitmapPaintButton.box.h = layout.bitmapBtnH;
      bitmapPaintButton.box.x = layout.bitmapPaintBtnX;
      bitmapPaintButton.box.y = layout.bitmapBtnY;
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

