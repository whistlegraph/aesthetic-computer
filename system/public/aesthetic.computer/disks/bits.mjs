// Bits, 23.03.14.13.46
// A simple confetti / speck`ing brush.
// (Created for testing `resize` and adding spatial navigation to the
//  nopaint system api)

/* #region ✅ TODO 
  + Done
  - [x] Clean up the api so it's replicable across other brushes...
  - [x] Make basic colored speck functionality.
  - [x] Make it work with a resized painting.
#endregion */

// 🎨 Paint (Executes every display frame)
function paint({
  system,
  colon,
  pen,
  page,
  screen,
  api,
  num: { randIntRange: rr },
}) {
  if (pen?.drawing) {
    if (colon[0] === "shake")
      system.nopaint.translate(api, rr(-1, 1), rr(-1, 1)); // 📳 Shakey bits!
    const { brush } = system.nopaint.present(api),
      d = 8,
      s = rr(1, 3);
    page(system.painting)
      .ink()
      .box(brush.x + rr(-d, d), brush.y + rr(-d, d), s, "fill*center")
      .page(screen);
  }
}

// 📚 Library (Useful functions used throughout the piece)
export const system = "nopaint";
export { paint };
