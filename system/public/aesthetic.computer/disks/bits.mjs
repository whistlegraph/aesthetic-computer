// Bits, 23.03.14.13.46
// A simple confetti / speck`ing brush.

/* #region ✅ TODO 
  - [🟡] Make it work with a resized painting.
  + Done
  - [x] Make basic colored speck functionality.
#endregion */

// 🥾 Boot (Runs once before first paint and sim)
// function boot({ wipe, ink, screen }) {
// }

// 🎨 Paint (Executes every display frame)
function paint({
  pen,
  ink,
  screen: { width, height },
  num: { randIntRange: rr },
}) {
  if (pen?.drawing) {
    const d = 32;
    ink().box(pen.x + rr(-d, d), pen.y + rr(-d, d), rr(1, 9), "fill*center");
  }
}

// 📚 Library (Useful functions used throughout the piece)
// ...

export const system = "nopaint";
export { paint };
