// No Paint
// This module contains *most* of the nopaint system template functionality.
// Shared functionality can be found in `disk.mjs`.

let state = "idle";

// Used when defining a custom piece functions in a nopaint system brush to
// inherit common behavior.
function nopaint_boot({ api, screen, system, painting, store }) {
  nopaint_adjust(screen, system, painting, store);
  system.nopaint.present(api);
}

function storeTransform(store, sys) {
  store["painting:transform"] = { translation: sys.nopaint.translation };
  store.persist("painting:transform", "local:db");
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
  api,
  jump,
  debug,
}) {
  if (e.is("keyboard:down:enter")) {
    download(`painting-${num.timestamp()}.png`, system.painting, {
      scale: 6,
      cropToScreen: true,
    });
  }

  // 🖌️ Painting

  // Start
  if (e.is("touch:1")) {
    state = "painting";
    system.nopaint.updateBrush(api);
    if (debug) console.log("🖌️ Painting!");
  }

  // Track
  if (nopaint_is("painting") && (e.is("move") || e.is("draw"))) {
    // if (debug) console.log("Updating brush...");
    system.nopaint.updateBrush(api);

  // Stop
  if (
    nopaint_is("painting") &&
    e.is("lift") &&
    (e.device === "mouse" || pens().length === 0)
  ) {
    state = "idle";
    system.nopaint.needsBake = true;
    if (debug) console.log("🖌️ Not painting...");
  }

  // 🧭 Panning (held 'alt' key or two finger drag)

  // Start
  if (
    e.is("keyboard:down:alt") ||
    ((e.is("touch:2") || e.is("touch:1")) && pens().length === 2)
  ) {
    if (debug) console.log("🧭 Panning!");
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
    (e.is("keyboard:up:alt") || e.is("lift:2") || e.is("lift:1"))
  ) {
    if (debug) console.log("🧭 Not panning...");
    state = "idle";
    storeTransform(store, system); // Store the translation after completion.
  }

  // Reset: By holding shift while alt is pressed down.
  if (
    nopaint_is("panning") &&
    (e.is("keyboard:down:shift") || e.is("touch:3"))
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
  if (e.is("keyboard:down:n") && !loading) {
    system.nopaint.abort();
    jump("prompt");
  }

  // Paint and then go to the prompt, same as default behavior "`".
  if (e.is("keyboard:down:p") && !loading) jump("prompt");
}

// 📚 Library
// Adjust painting resolution dynamically to match the screen,
// or provide a custom resolution.
// (Also used in `prompt`.)
function nopaint_adjust(screen, sys, painting, store, size = null) {
  if (!size && store["painting:resolution-lock"] === true) return;

  if (
    !size &&
    (sys.nopaint.translation.x !== 0 || sys.nopaint.translation.y !== 0)
  )
    return; // Never auto-resize if we are panned.

  const width = size?.w || screen.width;
  const height = size?.h || screen.height;
  sys.painting = painting(width, height, (p) => {
    p.wipe(64).paste(sys.painting);
  });
  store["painting"] = sys.painting;

  // Set a flag to prevent auto-resize.
  if (size) {
    store["painting:resolution-lock"] = true;
    store.persist("painting:resolution-lock", "local:db");
    store.persist("painting", "local:db"); // Also persist the painting.
    sys.nopaint.translation = { x: 0, y: 0 }; // Reset the transform.
    sys.nopaint.resetTransform({ system: sys, screen }); // Reset transform.
    storeTransform(store, sys);
  }
}

export { nopaint_boot, nopaint_act, nopaint_is, nopaint_adjust };
