// Bits, 23.03.14.13.46
// A simple confetti / speck`ing brush.

/* #region ✅ TODO 
  - [🟡] Make it work with a resized painting.

  + Done
  - [x] Make basic colored speck functionality.
#endregion */

// 🎨 Paint (Executes every display frame)
function paint({
  pen,
  wipe,
  system,
  page,
  paste,
  screen,
  num: { randIntRange: rr },
}) {
  if (pen?.drawing) {
    wipe(64); // Add a backdrop...
    const { x, y } = system.nopaint.display({ screen, system, paste });

    const d = 8,
      s = rr(1, 3);
    page(system.painting)
      .pan(pen.x - x, pen.y - y)
      .ink()
      .box(rr(-d, d) - x, rr(-d, d) - y, s, "fill*center")
      .unpan()
      .page(screen);
  }
}

// 📚 Library (Useful functions used throughout the piece)
// ...

export const system = "nopaint";
export { paint };
