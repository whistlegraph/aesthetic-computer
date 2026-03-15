// claude.mjs → renamed to "code" in the UI
// If credentials exist → launch claude directly.
// If not → launch "claude auth login" in terminal first, then claude.
// The terminal piece detects OAuth URLs and renders QR codes automatically.

function boot({ system }) {
  console.log("[code] boot — jumping to terminal:claude");
  // Always go straight to terminal with claude.
  // If auth is needed, claude itself will handle it and print a URL.
  // The terminal piece auto-detects URLs and renders QR codes.
  system.jump("terminal:claude");
}

export { boot };
