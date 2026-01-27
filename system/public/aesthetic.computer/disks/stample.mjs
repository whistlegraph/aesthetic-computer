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
      bitmapSize = min(BITMAP_MAX_SIZE, max(BITMAP_MIN_SIZE, floor(screen.width * 0.35)));
      bitmapColumnW = 0; // No column, overlaid
      bitmapX = screen.width - bitmapSize - BITMAP_MARGIN;
      bitmapY = screen.height - bottomBar - bitmapSize - BITMAP_BTN_HEIGHT * 2 - BITMAP_BTN_GAP * 2 - BITMAP_MARGIN;
      bitmapBtnW = bitmapSize;
      bitmapBtnY = bitmapY + bitmapSize + BITMAP_BTN_GAP;
    }
  }
  
  // Calculate strip button dimensions
  const stripAreaW = max(MIN_STRIP_WIDTH, screen.width - bitmapColumnW);
  const stripAreaH = availableHeight;
  const stripH = max(MIN_STRIP_HEIGHT, floor(stripAreaH / patCount));
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
  // const name = params[0] || "startup";
  const name = "startup"; // TODO: Recall previous samples from `store`.
  if (params[0]) {
    const rawParam = params[0];
    const decodedParam = rawParam.startsWith("%23")
      ? `#${rawParam.slice(3)}`
      : decodeURIComponent(rawParam);
    if (decodedParam.startsWith("#")) {
      bitmapLoading = true;
      bitmapLoaded = false;
      bitmapPreview = null;
      bitmapMeta = null;
      await loadPaintingCode(decodedParam, {
        get,
        preload,
        store,
        sound: { registerSample, sampleRate },
      });
    } else if (decodedParam === "painting" || decodedParam === "p") {
      bitmapLoading = true;
      bitmapLoaded = false;
      bitmapPreview = null;
      bitmapMeta = null;
      await loadSystemPainting({
        system,
        store,
        sound: { registerSample, sampleRate },
      });
    } else {
      const parsedPats = parseInt(decodedParam);
      if (!Number.isNaN(parsedPats)) pats = parsedPats;
    }
  }
  
  // Only load default sample if we didn't load from a #code bitmap
  if (!bitmapLoaded) {
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

  getSampleData(sampleId).then((data) => {
    if (bitmapLoaded) return;
    sampleData = data;
    // console.log("🔴 Sample Data:", sampleData);
  });
}

function sim({ sound }) {
  sounds.forEach((sound, index) => {
    // Get progress data.
    sound?.progress().then((p) => (progressions[index] = p.progress));
  });

  // Track bitmap playback progress for scrubber from any active sound source
  const activeSound = bitmapLoopSound || bitmapPlaySound || sounds.find(s => s);
  if (activeSound) {
    activeSound?.progress?.().then((p) => {
      bitmapProgress = p?.progress || 0;
    });
  } else {
    bitmapProgress = 0;
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
      // const prog = (1 - sounds[index].from)
      // console.log("need from:", sounds[index].options.from);
      // const prog = sounds[index]?.options.from || 0; // / progressions[index];

      let prog = 0;
      if (sounds[index]?.options.speed < 0) {
        prog = sounds[index].options.from;
      } else if (sounds[index]?.options.speed > 0) {
        prog = sounds[index].options.from;
      }

      let options = sounds[index]?.options;

      if (options) {
        // console.log(
        //   "From:",
        //   options.from,
        //   "To:",
        //   options.to,
        //   "Speed:",
        //   sounds[index]?.options.speed,
        // );

        const space = prog * btn.box.h;
        const negative = btn.box.h - space;
        let startY, height;

        if (options.speed > 0 || !options.speed) {
          // startY = btn.box.y;
          // console.log(options.to, options.from);
          // startY = btn.box.y + (1 - options.to) * btn.box.h;
          height = (1 - options.from) * btn.box.h;
          height = btn.box.h;
          startY = btn.box.y;
        } else {
          startY = btn.box.y + (1 - options.to) * btn.box.h;
          height = options.to * btn.box.h;
        }

        // console.log(
        //   "StartY",
        //   startY,
        //   "Height",
        //   height,
        //   "From:",
        //   options.from,
        //   "To:",
        //   options.to,
        // );

        if (progressions[index]) {
          ink("magenta").line(
            0,
            startY /* + 2*/,
            layout.stripW,
            startY /* + 2*/,
          );
          // console.log(startY);

          // ink("green", 64).box(
          //   btn.box.x,
          //   startY, // btn.box.y + btn.box.h,
          //   btn.box.w,
          //   height,
          //   // -btn.box.h * prog - progressions[index] * negative, // progressions[index],
          // );

          let y;
          let basey;
          const originaly = layout.topBar; // Use layout-based top bar height
          if (options.speed > 0 || !options.speed) {
            basey = floor(
              originaly + (1 - options.from) * (btn.box.h * btns.length),
            ); // btn.box.y + (1 - options.from) * btn.box.h;

            y =
              btn.box.y +
              /* (1 - options.from) */ 1 *
                (1 - progressions[index]) *
                btn.box.h;

            // console.log(basey);
            //y =
            //  btn.box.y +
            //  (1 - options.from / (1 - progressions[index])) * btn.box.h;
          } else {
            basey = btn.box.y + (1 - options.to) * btn.box.h;
            y = btn.box.y + (1 - options.to * progressions[index]) * btn.box.h;
          }

          ink("orange").line(0, y, layout.stripW, y);
          ink("blue").line(0, basey, layout.stripW, basey);
          // ink("lime").line(0, 100, btn.box.x + btn.box.w, 100);

          // const y =
          // btn.box.y + btn.box.h * (1 - prog) - progressions[index] * negative;
          // const y = btn.box.y + btn.box.h * (1 - progressions[index]);
          // ink("red").line(0, y, btn.box.x + btn.box.w, y);
        }
      }

      ink("black").write(
        sfxToKey[btns.length - 1 - index],
        btn.box.x + 4,
        btn.box.y + 4,
      );
    });

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
  });

  patsButton.paint((btn) => {
    ink("yellow", btn.down ? 128 : 64).box(btn.box);
  });

  notepatButton.paint((btn) => {
    ink("cyan", btn.down ? 160 : 90).box(btn.box);
    ink("black").box(btn.box, "out");
    ink("black").write("pat", btn.box.x + 6, btn.box.y + 6);
  });

  bitmapLoopButton?.paint((btn) => {
    const hasBitmap = !!bitmapPreview?.pixels?.length;
    const label = bitmapLooping ? "Stop" : "Loop";
    const bg = hasBitmap ? (bitmapLooping ? "magenta" : "purple") : "gray";
    ink(bg, btn.down ? 200 : 120).box(btn.box);
    ink("black").box(btn.box, "out");
    ink("white").write(
      label,
      btn.box.x + btn.box.w / 2 - text.width(label) / 2,
      btn.box.y + btn.box.h / 2 - text.height(label) / 2,
    );
  });

  bitmapPaintButton?.paint((btn) => {
    const hasBitmap = !!bitmapPreview?.pixels?.length;
    const label = "Paint";
    const bg = hasBitmap ? "lime" : "gray";
    ink(bg, btn.down ? 200 : 120).box(btn.box);
    ink("black").box(btn.box, "out");
    ink("black").write(
      label,
      btn.box.x + btn.box.w / 2 - text.width(label) / 2,
      btn.box.y + btn.box.h / 2 - text.height(label) / 2,
    );
  });

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
          // console.log("Playing sound index:", index);
          if (sound.microphone.connected) sound.microphone.disconnect();
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
          // if (downs[note]) return false;
          //if (btn.box.contains(e.dragBox)) {
          //if (
          //  btn.downPointer === 0 ||
          //  (e.pointer === btn.downPointer && btn.box.contains(e))
          //) {

          if (e.pointer === btn.downPointer || opts?.keyboard) {
            sounds[index]?.kill(0.1);
            delete btnSounds[index];
            return true;
          } else {
            return false;
          }

          // return true;
          //} else {
          // return e.pointer !== btn.downPointer;
          //}
          //}
          // console.log("Killing sound index:", index, sounds[index]);
        },
        cancel: (btn) => {
          // Kill the sound when button is cancelled (e.g., mouse leaves screen)
          sounds[index]?.kill(0.1);
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
            // console.log(`Pitch shift ${index}:`, e.delta.x);
            sounds[index]?.update({ shift: 0.03 * -e.delta.y });
            // sound.play(startupSfx, { pitch: freq(tone) });
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

export { boot, paint, act, sim };

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
  
  // Loop button
  bitmapLoopButton.box.w = layout.bitmapBtnW || layout.bitmapSize || 80;
  bitmapLoopButton.box.h = BITMAP_BTN_HEIGHT;
  bitmapLoopButton.box.x = layout.bitmapX;
  bitmapLoopButton.box.y = layout.bitmapBtnY;

  // Paint button (above loop button)
  if (bitmapPaintButton) {
    bitmapPaintButton.box.w = bitmapLoopButton.box.w;
    bitmapPaintButton.box.h = BITMAP_BTN_HEIGHT;
    bitmapPaintButton.box.x = bitmapLoopButton.box.x;
    bitmapPaintButton.box.y = bitmapLoopButton.box.y + BITMAP_BTN_HEIGHT + BITMAP_BTN_GAP;
  }
}

// RGB encoding: 3 samples per pixel (R, G, B channels)
// This gives 3x data density compared to grayscale
// Visual enhancement: Add slight color tint based on sample rate of change
function encodeSampleToBitmap(data, width = 256) {
  if (!Array.isArray(data) || data.length === 0) return null;
  const sampleLength = data.length;
  const samplesPerPixel = 3; // R, G, B
  const totalPixels = Math.ceil(sampleLength / samplesPerPixel);
  const height = Math.ceil(totalPixels / width);
  const pixels = new Uint8ClampedArray(width * height * 4);

  for (let i = 0; i < sampleLength; i += 1) {
    const v = Math.max(-1, Math.min(1, data[i]));
    const byte = Math.round((v + 1) * 127.5);
    const pixelIndex = Math.floor(i / samplesPerPixel);
    const channel = i % samplesPerPixel; // 0=R, 1=G, 2=B
    const idx = pixelIndex * 4 + channel;
    pixels[idx] = byte;
    // Set alpha to 255 for each pixel (only once per pixel)
    if (channel === 0) pixels[pixelIndex * 4 + 3] = 255;
  }

  return { width, height, pixels, sampleLength };
}

// RGB decoding: 3 samples per pixel (R, G, B channels)
function decodeBitmapToSample(bitmap, meta) {
  if (!bitmap?.pixels?.length || !bitmap?.width || !bitmap?.height) return null;
  const totalPixels = bitmap.width * bitmap.height;
  const samplesPerPixel = 3; // R, G, B
  const maxSamples = totalPixels * samplesPerPixel;
  const sampleLength = Math.min(meta?.sampleLength || maxSamples, maxSamples);
  const data = new Array(sampleLength);

  for (let i = 0; i < sampleLength; i += 1) {
    const pixelIndex = Math.floor(i / samplesPerPixel);
    const channel = i % samplesPerPixel; // 0=R, 1=G, 2=B
    const byte = bitmap.pixels[pixelIndex * 4 + channel] || 0;
    data[i] = byte / 127.5 - 1;
  }

  return data;
}

async function loadPaintingCode(code, { get, preload, store, sound }) {
  if (!code) return;
  const normalized = decodeURIComponent(code).replace(/^#/, "");
  const baseUrl =
    typeof location !== "undefined" && location.origin
      ? location.origin
      : "https://aesthetic.computer";

  // Fast path: short code direct media endpoint
  if (preload && normalized.length <= 6) {
    try {
      const directUrl = `${baseUrl}/media/paintings/${normalized}.png?t=${Date.now()}`;
      const directImg = await preload(directUrl, true);
      const directBuffer = await imageToBuffer(directImg);
      if (directBuffer?.pixels?.length) {
        bitmapPreview = {
          width: directBuffer.width,
          height: directBuffer.height,
          pixels: directBuffer.pixels,
        };
        // Update sampleLength to account for RGB encoding (3 samples per pixel)
        const totalPixels = directBuffer.width * directBuffer.height;
        bitmapMeta = {
          sampleLength: totalPixels * 3, // RGB = 3 samples per pixel
          sampleRate: sound?.sampleRate || 48000,
        };

        const decoded = decodeBitmapToSample(bitmapPreview, bitmapMeta);
        if (decoded?.length) {
          sampleData = decoded;
          sampleId = bitmapSampleId;
          sound?.registerSample?.(bitmapSampleId, decoded, bitmapMeta.sampleRate);
          bitmapLoaded = true;
          bitmapLoading = false;
        }
        return;
      }
    } catch (err) {
      console.warn("🖼️ Stample direct code load failed", err);
    }
  }

  let metadata = store?.[`painting-code:${normalized}`];
  if (!metadata?.slug || !metadata?.handle) {
    try {
      const response = await fetch(`/api/painting-code?code=${normalized}`);
      if (response.ok) {
        metadata = await response.json();
        if (metadata?.slug && metadata?.handle && store) {
          store[`painting-code:${normalized}`] = metadata;
        }
      }
    } catch (err) {
      console.warn("🖼️ Stample failed to resolve painting code", err);
      return;
    }
  }

  if (!metadata?.slug || !metadata?.handle) return;

  try {
    let img;

    if (preload) {
      const handle = metadata.handle.replace(/^@/, "");
      const base = `${baseUrl}/media/@${handle}/painting/${metadata.slug}.png`;
      const bust = `${base}?t=${Date.now()}`;
      img = await preload(bust, true);
    }

    if (!img && get?.painting) {
      const got = await get.painting(metadata.slug).by(metadata.handle);
      img = got?.img || got?.painting || got;
    }

    if (!img) return;

    const buffer = await imageToBuffer(img);
    if (!buffer?.pixels?.length) return;

    bitmapPreview = {
      width: buffer.width,
      height: buffer.height,
      pixels: buffer.pixels,
    };
    const totalPixels = buffer.width * buffer.height;
    bitmapMeta = {
      sampleLength: totalPixels * 3, // RGB = 3 samples per pixel
      sampleRate: sound?.sampleRate || 48000,
    };

    const decoded = decodeBitmapToSample(bitmapPreview, bitmapMeta);
    if (decoded?.length) {
      sampleData = decoded;
      sampleId = bitmapSampleId;
      sound?.registerSample?.(bitmapSampleId, decoded, bitmapMeta.sampleRate);
      bitmapLoaded = true;
      bitmapLoading = false;
    }
  } catch (err) {
    console.warn("🖼️ Stample failed to load painting", err);
    bitmapLoading = false;
  }
}

async function loadSystemPainting({ system, store, sound }) {
  let source =
    (system?.nopaint?.buffer?.pixels?.length && system?.nopaint?.buffer) ||
    system?.painting ||
    null;

  if (!source?.pixels?.length || !source?.width || !source?.height) {
    source = store?.painting || store?.["painting"] || null;
  }

  if (!source?.pixels?.length || !source?.width || !source?.height) {
    try {
      source = await store?.retrieve?.("painting", "local:db");
    } catch (err) {
      source = null;
    }
  }

  if (!source?.pixels?.length || !source?.width || !source?.height) {
    bitmapLoading = false;
    return;
  }

  bitmapPreview = {
    width: source.width,
    height: source.height,
    pixels: new Uint8ClampedArray(source.pixels),
  };

  const totalPixels = source.width * source.height;
  bitmapMeta = {
    sampleLength: totalPixels * 3, // RGB = 3 samples per pixel
    sampleRate: sound?.sampleRate || 48000,
  };

  const decoded = decodeBitmapToSample(bitmapPreview, bitmapMeta);
  if (decoded?.length) {
    sampleData = decoded;
    sampleId = bitmapSampleId;
    sound?.registerSample?.(bitmapSampleId, decoded, bitmapMeta.sampleRate);
    bitmapLoaded = true;
  }

  bitmapLoading = false;
}

async function imageToBuffer(image) {
  if (!image) return null;
  const source = image.img || image.bitmap || image;

  if (source?.pixels && source?.width && source?.height) {
    return {
      width: source.width,
      height: source.height,
      pixels: new Uint8ClampedArray(source.pixels),
    };
  }

  if (source?.data && source?.width && source?.height) {
    return {
      width: source.width,
      height: source.height,
      pixels: new Uint8ClampedArray(source.data),
    };
  }

  const width = source.width || source.naturalWidth || source.videoWidth;
  const height = source.height || source.naturalHeight || source.videoHeight;
  if (!width || !height) return null;

  let canvas;
  if (typeof OffscreenCanvas !== "undefined") {
    canvas = new OffscreenCanvas(width, height);
  } else if (typeof document !== "undefined") {
    canvas = document.createElement("canvas");
    canvas.width = width;
    canvas.height = height;
  }
  if (!canvas) return null;

  const ctx = canvas.getContext("2d");
  if (!ctx) return null;
  ctx.clearRect(0, 0, width, height);
  ctx.drawImage(source, 0, 0, width, height);
  const imageData = ctx.getImageData(0, 0, width, height);

  return {
    width,
    height,
    pixels: new Uint8ClampedArray(imageData.data),
  };
}
