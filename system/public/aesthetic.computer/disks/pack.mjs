// pack, 2026.02.13
// Generate a self-contained offline HTML pack for a KidLisp piece.
// Usage: pack~$code (e.g., pack~$bop)
// This loads the piece in a standalone HTML file with all dependencies embedded.

const { floor } = Math;

function boot({ api, params, jump, screen, user }) {
  const { wipe, ink, box, write } = api;

  // Get the $code from params (e.g., pack~$xyz → params[0] = "$xyz")
  let pieceCode = params[0];

  if (!pieceCode) {
    // No code provided, show error
    wipe("red");
    ink("white");
    write("Usage: pack~$code", { center: "xy" });
    return { needsPaint: false };
  }

  // Remove $ prefix if present
  if (pieceCode.startsWith("$")) {
    pieceCode = pieceCode.slice(1);
  }

  // Redirect to bundle API endpoint
  const host = api.net.host || "";
  const dev = host.startsWith("localhost") || host.includes("local.");
  const apiUrl = dev ? `https://${host}` : "https://aesthetic.computer";
  const bundleUrl = `${apiUrl}/api/pack-html?code=${encodeURIComponent(pieceCode)}`;

  // Navigate to pack endpoint (downloads HTML file)
  jump("out:" + bundleUrl);

  // Show loading message while redirecting
  wipe(32);
  ink("cyan");
  write("Packing offline HTML...", { center: "xy", size: 2 });

  return { needsPaint: false };
}

export { boot };
