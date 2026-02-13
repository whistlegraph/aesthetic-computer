// bundle, 2026.02.13
// Generate a self-contained offline HTML bundle for a KidLisp piece.
// Usage: bundle~$code (e.g., bundle~$bop)
// This loads the piece in a standalone HTML file with all dependencies embedded.

const { floor } = Math;

function boot({ api, params, jump, screen, user }) {
  const { wipe, ink, box, write } = api;

  // Get the $code from params (e.g., bundle~$xyz → params[0] = "$xyz")
  let pieceCode = params[0];

  if (!pieceCode) {
    // No code provided, show error
    wipe("red");
    ink("white");
    write("Usage: bundle~$code", { center: "xy" });
    return { needsPaint: false };
  }

  // Remove $ prefix if present
  if (pieceCode.startsWith("$")) {
    pieceCode = pieceCode.slice(1);
  }

  // Redirect to bundle API endpoint
  const dev = window.location.hostname === "localhost" ||
              window.location.hostname.includes("local.");
  const apiUrl = dev ? "https://localhost:8888" : "https://aesthetic.computer";
  const bundleUrl = `${apiUrl}/api/bundle-html?code=${encodeURIComponent(pieceCode)}`;

  // Redirect to bundle endpoint (downloads HTML file)
  window.location.href = bundleUrl;

  // Show loading message while redirecting
  wipe(32);
  ink("cyan");
  write("Generating offline bundle...", { center: "xy", size: 2 });

  return { needsPaint: false };
}

export { boot };
