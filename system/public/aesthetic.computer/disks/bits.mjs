// Bits, 23.03.14.13.46
// A simple confetti / speck`ing brush.
// (Created for testing `resize` and adding spatial navigation to the
//  nopaint system api)

/* #region ✅ TODO 
  - [] ...
  + Done
  - [x] Finalize bits api so it can move to `rect`.
  - [x] Clean up the api so it's replicable across other brushes.
  - [x] Make basic colored speck functionality.
  - [x] Make it work with a resized painting.
#endregion */

// 🎨 Paint (Executes every display frame)
function paint({
  system: sys,
  colon,
  pen,
  page,
  screen,
  api,
  num: { randIntRange: rr },
}) {
  if (pen?.drawing) {
    // 📳 Shakey bits!
    if (colon[0] === "shake") sys.nopaint.translate(api, rr(-1, 1), rr(-1, 1));
    const brush = sys.nopaint.brush,
      d = 8,
      s = rr(1, 3);

    // Render to the painting.
    page(sys.painting)
      .ink()
      .box(brush.x + rr(-d, d), brush.y + rr(-d, d), s, "fill*center")
      .page(screen); // Add colored bits to the painting.

    sys.nopaint.present(api); // Display the painting on the screen.
  }
}

// 📚 Library (Useful functions used throughout the piece)
export const system = "nopaint";
export { paint };
