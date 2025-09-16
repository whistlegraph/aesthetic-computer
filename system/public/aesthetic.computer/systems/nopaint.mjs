// No Paint
// This module contains *most* of the nopaint system template functionality.
// Shared functionality can be found in `disk.mjs`.

import { generateNopaintHUDLabel } from "../lib/color-highlighting.mjs";

let state = "idle";
let previousState = "idle"; // Track state before panning to restore it later

// 📊 Flash effect for bake events
let bakeFlashTime = 0;
let bakeFlashDuration = 300; // Flash duration in milliseconds
let needsPaintRef = null; // Store reference to needsPaint function
let animationId = null; // Store animation frame ID
const cursor = { x: 0, y: 0 };

// Enhanced logging system for debugging touch behavior
let sessionId = null;
let logBuffer = [];
const LOG_BUFFER_SIZE = 100;
let currentStrokePointCount = 0; // Track points in current stroke only

// Initialize session logging
function initSessionLogging(api) {
  if (!sessionId) {
    sessionId = `session_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
    log(api, 'SESSION_START', { sessionId, timestamp: Date.now(), userAgent: navigator.userAgent });
  }
}

// Enhanced logging function with device-specific file handling
function log(api, event, data = {}) {
  const timestamp = Date.now();
  const logEntry = {
    timestamp,
    sessionId,
    event,
    data: { ...data }
  };

  // Add to buffer
  logBuffer.push(logEntry);
  if (logBuffer.length > LOG_BUFFER_SIZE) {
    logBuffer.shift(); // Remove oldest entry
  }

  // Send to server with device-specific filename
  const device = data.device || 'unknown';
  const filename = `${device}_touch_debug.log`;

  try {
    api.net.log(`/tmp/dev-logs/${filename}`, JSON.stringify(logEntry));
  } catch (error) {
    console.error('Failed to send log:', error);
  }
}

// Used when defining a custom piece functions in a nopaint system brush to
// inherit common behavior.
function nopaint_boot({ api, screen, system, painting, store }) {
  cursor.x = screen.width / 2;
  cursor.y = screen.height / 2;
  nopaint_adjust(api);

  // Initialize session logging
  initSessionLogging(api);

  system.nopaint.buffer = painting(
    system.painting.width,
    system.painting.height,
    (p) => {
      p.wipe(255, 255, 255, 0);
    },
  );

  system.nopaint.present(api);
}

function nopaint_is(stateQuery) {
  return state === stateQuery;
}

// 📊 Trigger bake flash effect
function nopaint_triggerBakeFlash() {
  bakeFlashTime = performance.now();
  console.log("📊 BAKE FLASH: Triggered at", bakeFlashTime);
  
  // Cancel any existing animation
  if (animationId) {
    cancelAnimationFrame(animationId);
  }
  
  // Start continuous animation using requestAnimationFrame
  const animateFlash = () => {
    const currentTime = performance.now();
    const timeSinceBake = currentTime - bakeFlashTime;
    
    if (timeSinceBake < bakeFlashDuration) {
      // Still flashing, continue animation and trigger needsPaint
      if (needsPaintRef) {
        needsPaintRef();
      }
      animationId = requestAnimationFrame(animateFlash);
    } else {
      // Flash finished
      animationId = null;
    }
  };
  
  // Start the animation
  animationId = requestAnimationFrame(animateFlash);
}

function nopaint_act({
  event: e,
  download,
  screen,
  system,
  painting,
  loading,
  store,
  pens,
  pen,
  api,
  num,
  jump,
  debug,
}) {
  // Debug logging: Log ALL events with device info to see what's actually happening
  if (e.device && (e.device === "touch" || e.device === "pen" || e.device !== "mouse")) {
    log(api, 'RAW_EVENT', {
      device: e.device,
      eventType: e.type || 'unknown',
      touchState: e.is ? `touch:${e.is("touch:1") ? "1" : e.is("touch:2") ? "2" : "none"}` : 'no-is-method',
      liftState: e.is ? `lift:${e.is("lift:1") ? "1" : e.is("lift:2") ? "2" : "none"}` : 'no-is-method',
      drawState: e.is ? `draw:${e.is("draw:1") ? "1" : "none"}` : 'no-is-method',
      rawX: e.x,
      rawY: e.y,
      currentPaintingState: nopaint_is("painting"),
      timestamp: num.timestamp()
    });
  }

  // 🚨 CRITICAL DEBUG: Log ANY lift event to track state issues
  if (e.is && (e.is("lift:1") || e.is("lift:2"))) {
    console.log("🚨 LIFT EVENT DEBUG:", {
      currentState: state,
      previousState: previousState,
      isPainting: nopaint_is("painting"),
      isPanning: nopaint_is("panning"),
      device: e.device,
      liftType: e.is("lift:1") ? "lift:1" : "lift:2",
      willProcessInPaintingBlock: nopaint_is("painting") && e.is("lift:1") && (e.device === "mouse" || e.device === "pen" || e.device === "touch"),
      willProcessInPanningBlock: nopaint_is("panning") && (e.is("keyboard:up:shift") || e.is("lift:2") || e.is("lift:1"))
    });
  }

  // if (e.is("keyboard:down:enter")) {
  //   download(`painting-${num.timestamp()}.png`, system.painting, {
  //     scale: 6,
  //     cropToScreen: true,
  //   });
  // }

  // 🖌️ Painting

  // Start
  // console.log(e);
  // TODO: Fix artifacts that occur while touching to draw, after using the
  //       pen.

  // 🔥
  // TODO: Add each of these to the record if it exists..

  if (e.is("touch:1")) {
    const timestamp = performance.now();
    
    console.log(`🎯 STATE DEBUG [${Math.floor(timestamp)}]: Touch detected, changing state from`, state, "to 'painting'", {
      currentState: state,
      eventDevice: e.device,
      eventType: e.type,
      eventIs: {
        touch1: e.is("touch:1")
      },
      timestamp: timestamp
    });
    
    state = "painting";
    
    console.log(`🎯 STATE DEBUG [${Math.floor(timestamp)}]: State changed to:`, state, `nopaint_is("painting"):`, nopaint_is("painting"));
    
    system.nopaint.updateBrush(api, "touch");

    // Reset stroke point count for new stroke
    currentStrokePointCount = 0;

    // Enhanced logging for touch events
    log(api, 'TOUCH_START', {
      device: e.device,
      rawX: e.x,
      rawY: e.y,
      brushX: system.nopaint.brush.x,
      brushY: system.nopaint.brush.y,
      state: state,
      timestamp: num.timestamp()
    });

    // if (debug) console.log("🖌️ Painting!");

    // TODO:
    // 🔥 Add this to the current gestures, which will be packed
    //    when `addToRecord` is run.
    system.nopaint.gestureRecord.push([
      num.timestamp(),
      "touch:1",
      system.nopaint.brush.x,
      system.nopaint.brush.y,
    ]);
  }

  // Track
  if (nopaint_is("painting") && e.is("draw:1")) {
    system.nopaint.updateBrush(api, "draw");

    const rec = system.nopaint.gestureRecord;

    if (
      system.nopaint.brush.x !== rec[rec.length - 1][2] ||
      system.nopaint.brush.y !== rec[rec.length - 1][3]
    ) {
      // Increment stroke-specific point count
      currentStrokePointCount++;

      // Enhanced logging for draw events
      log(api, 'DRAW_POINT', {
        device: e.device,
        rawX: e.x,
        rawY: e.y,
        brushX: system.nopaint.brush.x,
        brushY: system.nopaint.brush.y,
        state: state,
        pointCount: currentStrokePointCount,
        timestamp: num.timestamp()
      });

      rec.push([
        num.timestamp(),
        "draw:1",
        system.nopaint.brush.x,
        system.nopaint.brush.y,
      ]);
    }
  }

  // Stop
  if (
    nopaint_is("painting") &&
    e.is("lift:1") &&
    (e.device === "mouse" || e.device === "pen" || e.device === "touch")
  ) {
    const timestamp = performance.now();
    
    console.log(`🎯 STATE DEBUG [${Math.floor(timestamp)}]: Lift detected, changing state from 'painting' to 'idle'`, {
      currentState: state,
      eventDevice: e.device,
      eventType: e.type,
      eventIs: {
        lift1: e.is("lift:1"),
        painting: nopaint_is("painting")
      },
      timestamp: timestamp
    });
    
    state = "idle";
    
    console.log(`🎯 STATE DEBUG [${Math.floor(timestamp)}]: State changed to:`, state, `nopaint_is("painting"):`, nopaint_is("painting"));
    
    if (!system.nopaint.bakeOnLeave) system.nopaint.needsBake = true;

    // 📊 Trigger needsPaint after lift to ensure HUD updates
    api.needsPaint();

    // Enhanced logging for lift events
    log(api, 'TOUCH_END', {
      device: e.device,
      rawX: e.x,
      rawY: e.y,
      brushX: system.nopaint.brush.x,
      brushY: system.nopaint.brush.y,
      previousState: 'painting',
      newState: state,
      gestureLength: currentStrokePointCount + 1, // +1 to include the final point
      timestamp: num.timestamp()
    });

    // if (debug) console.log("🖌️ Not painting...");
    system.nopaint.gestureRecord.push([
      num.timestamp(),
      "lift:1",
      system.nopaint.brush.x,
      system.nopaint.brush.y,
    ]);

    // 🎯 PRESERVE coordinates for baking before clearing state
    // This fixes the geometry mismatch issue where bake() had no access to dragBox coordinates
    console.log("🎯 NOPAINT: Preserving coordinates before state clearing");
    if (system.nopaint.brush && system.nopaint.brush.dragBox) {
      system.nopaint.finalDragBox = {
        x: system.nopaint.brush.dragBox.x,
        y: system.nopaint.brush.dragBox.y,
        w: system.nopaint.brush.dragBox.w,
        h: system.nopaint.brush.dragBox.h
      };
      console.log("🎯 NOPAINT: Preserved finalDragBox:", system.nopaint.finalDragBox);
    } else {
      console.log("🎯 NOPAINT: No dragBox to preserve");
    }
    if (system.nopaint.startDrag) {
      system.nopaint.finalStartDrag = {
        x: system.nopaint.startDrag.x,
        y: system.nopaint.startDrag.y
      };
      console.log("🎯 NOPAINT: Preserved finalStartDrag:", system.nopaint.finalStartDrag);
    } else {
      console.log("🎯 NOPAINT: No startDrag to preserve");
    }

    // Clear the brush and dragBox state to prevent flickering between gestures
    system.nopaint.brush = null;
    system.nopaint.startDrag = null;
  }

  // 🔭 Zooming...

  if (e.is("move")) {
    cursor.x = pen.x;
    cursor.y = pen.y;
    
    // 📊 Update brush and trigger needsPaint for HUD updates during move (even when lifted)
    system.nopaint.updateBrush(api, "move");
    api.needsPaint();
  }

  if (e.is("keyboard:down:arrowup")) {
    // console.log("Zoom in...");
    system.nopaint.zoom(api, "in", cursor);
    system.nopaint.present(api);
    // Trigger live preview update if we have an active dragBox (ongoing rect operation)
    if (system.nopaint.brush?.dragBox) {
      api.needsPaint();
    }
  }

  if (e.is("keyboard:down:arrowdown")) {
    // console.log("Zoom out...");
    system.nopaint.zoom(api, "out", cursor);
    system.nopaint.present(api);
    // Trigger live preview update if we have an active dragBox (ongoing rect operation)
    if (system.nopaint.brush?.dragBox) {
      api.needsPaint();
    }
  }

  // 🧭 Panning (held 'shift' key or two finger drag)

  // Start
  if (
    e.is("keyboard:down:shift") ||
    ((e.is("touch:2") || e.is("touch:1")) && pens().length === 2)
  ) {
    console.log("🎯 STATE DEBUG: Panning start detected", {
      currentState: state,
      willSaveToPreviousState: state,
      eventDevice: e.device,
      eventType: e.type,
      eventIs: {
        keyboardDownShift: e.is("keyboard:down:shift"),
        touch1: e.is("touch:1"),
        touch2: e.is("touch:2")
      },
      pensLength: pens().length
    });
    
    // if (debug) console.log("🧭 Panning!");
    previousState = state; // Preserve current state
    state = "panning";
    
    console.log("🎯 STATE DEBUG: Panning started, state changed to:", state, "previousState:", previousState);
  }

  // Track
  if (
    nopaint_is("panning") &&
    ((e.is("move") && e.device === "mouse") || e.is("draw"))
  ) {
    system.nopaint.translate(api, e.delta.x, e.delta.y);

    const p = pens();
    if (p.length === 2) {
      // console.log(p); // TODO: Set the zoom level here.
    }

    system.nopaint.present(api);
    // Always trigger needsPaint during panning to ensure visual updates
    api.needsPaint();
  }

  // End
  if (
    nopaint_is("panning") &&
    (e.is("keyboard:up:shift") || e.is("lift:2") || e.is("lift:1"))
  ) {
    console.log("🎯 STATE DEBUG: Panning end detected, restoring state", {
      currentState: state,
      previousState: previousState,
      eventDevice: e.device,
      eventType: e.type,
      eventIs: {
        keyboardUpShift: e.is("keyboard:up:shift"),
        lift1: e.is("lift:1"),
        lift2: e.is("lift:2")
      }
    });
    
    // 🚨 SAFEGUARD: If this is a lift event and previousState was "painting", 
    // we should go to "idle" instead to prevent stuck painting state
    if ((e.is("lift:1") || e.is("lift:2")) && previousState === "painting") {
      console.log("🚨 SAFEGUARD: Lift during panning with painting previousState - forcing to idle");
      state = "idle";
    } else {
      // if (debug) console.log("🧭 Not panning...");
      state = previousState; // Restore previous state
    }
    
    console.log("🎯 STATE DEBUG: State restored to:", state, "from previousState:", previousState);
    
    previousState = "idle"; // Reset
    system.nopaint.storeTransform(store, system); // Store the translation after completion.
  }

  // Reset: By holding `alt` while `shift` (aka meta) is pressed down.
  if (
    nopaint_is("panning") &&
    (e.is("keyboard:down:meta") || e.is("touch:3"))
  ) {
    state = previousState; // Restore previous state
    previousState = "idle"; // Reset
    system.nopaint.resetTransform(api);
    system.nopaint.present(api);
  }

  // Auto-resizing...
  if (e.is("reframed")) {
    nopaint_adjust(api);
    system.nopaint.present(api);
  }

  // No and then return to the prompt.
  // if (e.is("keyboard:down:n") && !loading) {
  //   system.nopaint.abort();
  //   jump("prompt");
  // }

  // Paint and then go to the prompt, same as default behavior "`".
  // if (e.is("keyboard:down:p") && !loading) jump("prompt");
}

// 📚 Library
// Adjust painting resolution dynamically to match the screen,
// or provide a custom resolution.
// (Also used in `prompt`.)
function nopaint_adjust(
  api,
  size = null,
  slug = "resize",
) {
  const { screen, system: sys, painting, store, dark, theme } = api;
  if (!size && store["painting:resolution-lock"] === true) return;

  if (
    !size &&
    (sys.nopaint.translation.x !== 0 || sys.nopaint.translation.y !== 0)
  ) {
    return; // Stop auto-resizing if we are panned.
  }

  let resizing = false;

  if (!size) {
    // Assume we are auto-resizing now.
    if (sys.nopaint.undo.paintings.length === 0) {
      size = { w: screen.width, h: screen.height };
    } else {
      size = {
        w: Math.max(screen.width, sys.painting?.width || 0),
        h: Math.max(screen.height, sys.painting?.height || 0),
      };
    }

    resizing = true;
  }

  // TODO: Add auto-resizing back to paintings.

  if (size || !sys.painting) {
    // Check to see if size?.w has an x at the end.
    let width, height;
    if (size.w && size.h) {
      // Allow for "2x or 3x" modifiers.

      if (typeof size.w === "string") {
        width = size.w.endsWith("x")
          ? parseFloat(size.w.slice(0, -1)) *
          (sys.painting?.width || screen.width)
          : parseInt(size.w);
      } else {
        width = size.w; // Assume number.
      }

      if (typeof size.h === "string") {
        height = size.h.endsWith("x")
          ? parseFloat(size.h.slice(0, -1)) *
          (sys.painting?.height || screen.height)
          : parseInt(size.h);
      } else {
        height = size.h; // Assume number.
      }
    } else {
      width = screen.width;
      height = screen.height;
    }

    if (isNaN(width) || isNaN(height)) return false;

    sys.painting = painting(width, height, (p) => {
      if (size?.scale) {
        p.paste(sys.painting, 0, 0, { width, height });
      } else {
        // TODO: Put in some custom noise function here...
        // Detect if we are in light or dark mode...
        // $common
        p.wipe(theme[dark ? "dark" : "light"].wipeNum);
        p.paste(sys.painting);
      }
    });

    store["painting"] = {
      width: sys.painting.width,
      height: sys.painting.height,
      pixels: sys.painting.pixels,
    }; // sys.painting;

    if (!resizing) sys.nopaint.addUndoPainting(sys.painting, slug);
  }

  // Set a flag to prevent auto-resize.
  if (size && !resizing) {
    store["painting:resolution-lock"] = true;
    store.persist("painting:resolution-lock", "local:db");
    store.persist("painting", "local:db"); // Also persist the painting.

    // 🪝 Frame-based monitoring will handle resize broadcast with storage completion hooks
    // No direct broadcast needed - the frame monitoring system will detect dimension changes
    // and use the proper storage completion sequence for cross-tab sync

    sys.nopaint.resetTransform({ system: sys, screen }); // Reset transform.
    sys.nopaint.storeTransform(store, sys);
  }
  return true;
}

// 🔍 Debug overlay function to be called during paint phase
function nopaint_paint({
  screen,
  ink,
  write,
  line,
  circle,
  box,
  system,
  needsPaint
}) {
  // Store reference to needsPaint for flash animations
  needsPaintRef = needsPaint;
  
  // Ensure needsPresent is set to trigger display updates
  if (system?.nopaint) {
    system.nopaint.needsPresent = true;
  }
  
  // 📊 ALWAYS return true to ensure continuous painting for performance HUD
  return true;
}

// 📊 Performance HUD for nopaint system pen state debugging
function nopaint_renderPerfHUD({
  ink,
  write,
  box,
  screen,
  pen,
  system,
  nopaintPerf
}) {
  if (!nopaintPerf || !ink || !write || !box) return;

  // 🎨 Trigger next frame for smooth animation
  if (needsPaintRef) {
    needsPaintRef();
  }

  // 📊 Console log current state for debugging
  // console.log("🟢 CURRENT STATE:", state, "| nopaint_is('painting'):", nopaint_is("painting"));

  const hudWidth = 90; // Reduced from 120 to make narrower
  const hudHeight = 80;
  const x = screen.width - hudWidth - 2;
  const y = 2;
  
  // 📊 Check if we should flash (bake effect)
  const currentTime = performance.now();
  const timeSinceBake = currentTime - bakeFlashTime;
  const isFlashing = timeSinceBake < bakeFlashDuration;
  
  // Semi-transparent background (flash bright when baking)
  if (isFlashing) {
    // Flash bright white/yellow during bake
    const flashIntensity = Math.max(0, 1 - (timeSinceBake / bakeFlashDuration)); // 1.0 to 0.0
    const flashAlpha = Math.floor(100 + (flashIntensity * 155)); // 100 to 255
    ink(255, 255, 200, flashAlpha);
  } else {
    ink(0, 0, 0, 200);
  }
  box(x - 1, y - 1, hudWidth + 2, hudHeight + 2, "fill");
  
  // Border (flash bright during bake)
  if (isFlashing) {
    const flashIntensity = Math.max(0, 1 - (timeSinceBake / bakeFlashDuration));
    const flashColor = Math.floor(100 + (flashIntensity * 155)); // 100 to 255
    ink(flashColor, 255, flashColor, 255);
  } else {
    ink(100, 255, 100, 255);
  }
  box(x - 1, y - 1, hudWidth + 2, hudHeight + 2, "outline");
  
  // Pen state data
  let lineY = y + 1;
  const lineHeight = 8;
  
  // Title
  ink(255, 255, 255);
  write("NOPAINT PERF", { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
  lineY += lineHeight + 2;
  
  if (pen) {
    // Pen coordinates
    ink(100, 200, 255);
    const penX = (typeof pen.x === 'number') ? pen.x : 0;
    const penY = (typeof pen.y === 'number') ? pen.y : 0;
    write(`X:${penX.toFixed(1)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
    lineY += lineHeight;
    
    ink(100, 200, 255);
    write(`Y:${penY.toFixed(1)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
    lineY += lineHeight;
    
    // Drag box info if available
    if (pen.dragBox && typeof pen.dragBox.x === 'number' && typeof pen.dragBox.y === 'number') {
      ink(255, 100, 200);
      write(`DRG X:${pen.dragBox.x.toFixed(1)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
      lineY += lineHeight;
      
      write(`DRG Y:${pen.dragBox.y.toFixed(1)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
      lineY += lineHeight;
      
      if (typeof pen.dragBox.w === 'number') {
        write(`DRG W:${pen.dragBox.w.toFixed(1)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
        lineY += lineHeight;
      }
      
      if (typeof pen.dragBox.h === 'number') {
        write(`DRG H:${pen.dragBox.h.toFixed(1)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
        lineY += lineHeight;
      }
    }
    
    // Additional pen state
    if (pen.pressure !== undefined && typeof pen.pressure === 'number') {
      ink(150, 150, 255);
      write(`PRESS:${pen.pressure.toFixed(2)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
      lineY += lineHeight;
    }
  } else {
    ink(255, 0, 0);
    write("NO PEN DATA", { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
  }
  
  // Nopaint system state if available
  if (system?.nopaint) {
    // Clean state display - the debugging proved the state management works correctly
    ink(255, 255, 0);
    write(`STATE:${state}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
    lineY += lineHeight;
    
    ink(200, 200, 0);
    write(`PREV:${previousState}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
    lineY += lineHeight;
    
    // Show current painting status with clear color coding
    const isPainting = nopaint_is("painting");
    ink(isPainting ? [0, 255, 0] : [255, 0, 0]);
    write(`PAINT:${isPainting ? "YES" : "NO"}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
    lineY += lineHeight;
    
    // Show timestamp for reference
    const now = performance.now();
    ink(128, 128, 128);
    write(`TIME:${Math.floor(now % 10000)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
  }
}

// Helper function for nopaint brushes to generate colored HUD labels
function nopaint_generateColoredLabel(brushName, colorParams, rawParams, modifiers = "", api) {
  if (!api || !api.hud || !api.hud.label) return;

  const coloredLabel = generateNopaintHUDLabel(brushName, colorParams, rawParams, modifiers);
  api.hud.label(coloredLabel);
}

export { nopaint_boot, nopaint_act, nopaint_paint, nopaint_is, nopaint_adjust, nopaint_generateColoredLabel, nopaint_renderPerfHUD, nopaint_triggerBakeFlash };
