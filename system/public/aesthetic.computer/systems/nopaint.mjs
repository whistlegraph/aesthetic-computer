// No Paint
// This module contains all of the nopaint system template functionality.

// Used when defining a custom piece functions in a nopaint system brush to
// inherit common behavior.
function nopaint_boot({ api, screen, system, painting, store }) {
  nopaint_adjust(screen, system, painting, store);
  system.nopaint.present(api);
}

let panning = false;

function storeTransform(store, sys) {
  store["painting:transform"] = { translation: sys.nopaint.translation };
  store.persist("painting:transform", "local:db");
}

function nopaint_act({
  event: e,
  download,
  screen,
  system,
  painting,
  loading,
  store,
  reload,
  api,
}) {
  if (e.is("keyboard:down:enter")) {
    download(`painting-${num.timestamp()}.png`, system.painting, {
      scale: 6,
      cropToScreen: true,
    });
  }

  // Panning (held 'alt' key or two finger drag)
  if (e.is("move") && panning) {
    system.nopaint.translate(api, e.delta.x, e.delta.y);
    system.nopaint.present(api);
  }

  if (e.is("move") || e.is("draw")) system.nopaint.updateBrush(api);

  if (e.is("keyboard:down:alt") || e.is("touch:2")) panning = true;

  // Reset pan by holding shift while alt is pressed down.
  if (panning && e.is("keyboard:down:shift")) {
    panning = false;
    system.nopaint.translation = { x: 0, y: 0 };
    system.nopaint.present(api);
  }

  if (e.is("keyboard:up:alt") || e.is("lift:2")) {
    panning = false;
    storeTransform(store, system); // Store the translation after completion.
  }

  if (e.is("reframed")) {
    nopaint_adjust(screen, system, painting, store);
    system.nopaint.present(api);
  }

  // No and then reload the same brush / reload without storing
  // the painting.
  if (e.is("keyboard:down:n") && !loading) {
    system.nopaint.abort();
    reload();
  }

  // Paint and then reload the same brush.
  if (e.is("keyboard:down:p") && !loading) reload();
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
    sys.nopaint.setTransform({ system: sys, screen }); // Reset transform.
    storeTransform(store, sys);
  }
}

export { nopaint_boot, nopaint_act, nopaint_adjust };
