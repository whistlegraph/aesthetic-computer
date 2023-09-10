// No Paint
// This module contains *most* of the nopaint system template functionality.
// Shared functionality can be found in `disk.mjs`.

let state = "idle";
const cursor = { x: 0, y: 0 };

// Used when defining a custom piece functions in a nopaint system brush to
// inherit common behavior.
function nopaint_boot({ api, screen, system, painting, store }) {
  cursor.x = screen.width / 2;
  cursor.y = screen.height / 2;
  nopaint_adjust(screen, system, painting, store);
  system.nopaint.present(api);
}

function nopaint_is(stateQuery) {
  return state === stateQuery;
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
    state = "painting";
    system.nopaint.updateBrush(api);
    // if (debug) console.log("🖌️ Painting!");

    // TODO:
    // 🔥 Add this to the current gestures, which will be packed
    //    when `addToRecord` is run.
    // system.nopaint.gestureRecord.push("gesture:start");
  }

  // Track
  // if (nopaint_is("painting") && (e.is("move") || e.is("draw"))) {
  if (nopaint_is("painting") && (e.is("move") || e.is("draw"))) {
    // if (debug) console.log("Updating brush...");
    system.nopaint.updateBrush(api);
    // TODO: system.nopaint.gestureRecord.push("gesture:mark");
  }

  // Stop
  if (
    nopaint_is("painting") &&
    e.is("lift") &&
    (e.device === "mouse" || pens().length === 0)
  ) {
    state = "idle";
    if (!system.nopaint.bakeOnLeave) system.nopaint.needsBake = true;
    if (debug) console.log("🖌️ Not painting...");
    // TODO: system.nopaint.gestureRecord.push("gesture:stop");
  }

  // 🔭 Zooming...

  if (e.is("move")) {
    cursor.x = pen.x;
    cursor.y = pen.y;
  }

  if (e.is("keyboard:down:arrowup")) {
    console.log("Zoom in...");
    system.nopaint.zoom(api, "in", cursor);
    system.nopaint.present(api);
  }

  if (e.is("keyboard:down:arrowdown")) {
    console.log("Zoom out...");
    system.nopaint.zoom(api, "out", cursor);
    system.nopaint.present(api);
  }

  // 🧭 Panning (held 'shift' key or two finger drag)

  // Start
  if (
    e.is("keyboard:down:shift") ||
    ((e.is("touch:2") || e.is("touch:1")) && pens().length === 2)
  ) {
    // if (debug) console.log("🧭 Panning!");
    state = "panning";
  }

  // Track
  if (nopaint_is("panning") && (e.is("move") || e.is("draw"))) {
    system.nopaint.translate(api, e.delta.x, e.delta.y);
    system.nopaint.present(api);
  }

  // End
  if (
    nopaint_is("panning") &&
    (e.is("keyboard:up:shift") || e.is("lift:2") || e.is("lift:1"))
  ) {
    // if (debug) console.log("🧭 Not panning...");
    state = "idle";
    system.nopaint.storeTransform(store, system); // Store the translation after completion.
  }

  // Reset: By holding `alt` while `shift` (aka meta) is pressed down.
  if (
    nopaint_is("panning") &&
    (e.is("keyboard:down:meta") || e.is("touch:3"))
  ) {
    state = "idle";
    system.nopaint.resetTransform(api);
    system.nopaint.present(api);
  }

  // Auto-resizing...
  if (e.is("reframed")) {
    nopaint_adjust(screen, system, painting, store);
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
  screen,
  sys,
  painting,
  store,
  size = null,
  slug = "resize",
) {
  if (!size && store["painting:resolution-lock"] === true) return;

  if (
    !size &&
    (sys.nopaint.translation.x !== 0 || sys.nopaint.translation.y !== 0)
  )
    return; // Stop auto-resizing if we are panned.

  if (size || !sys.painting) {
    // Check to see if size?.w has an x at the end.
    let width, height;
    if (size.w && size.h) {
      // Allow for "2x or 3x" modifiers.
      width = size.w.endsWith("x")
        ? parseFloat(size.w.slice(0, -1)) *
          (sys.painting?.width || screen.width)
        : parseInt(size.w);
      height = size.h.endsWith("x")
        ? parseFloat(size.h.slice(0, -1)) *
          (sys.painting?.height || screen.height)
        : parseInt(size.h);
    } else {
      width = screen.width;
      height = screen.height;
    }

    if (isNaN(width) || !isNaN(height)) return false;

    sys.painting = painting(width, height, (p) => {
      if (size?.scale) {
        p.paste(sys.painting, 0, 0, { width, height });
      } else {
        // TODO: Put in some custom noise function here...
        p.wipe(64).paste(sys.painting);
      }
    });

    store["painting"] = sys.painting;
    sys.nopaint.addUndoPainting(sys.painting, slug);
    return true;
  }

  // Set a flag to prevent auto-resize.
  if (size) {
    store["painting:resolution-lock"] = true;
    store.persist("painting:resolution-lock", "local:db");
    store.persist("painting", "local:db"); // Also persist the painting.
    sys.nopaint.resetTransform({ system: sys, screen }); // Reset transform.
    sys.nopaint.storeTransform(store, sys);
  }
}

export { nopaint_boot, nopaint_act, nopaint_is, nopaint_adjust };
