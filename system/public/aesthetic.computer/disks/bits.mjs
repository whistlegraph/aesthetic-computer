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
    wipe(0, 0, 200); // Is this necessary? 🟠
    const x = screen.width / 2 - system.painting.width / 2
    const y = screen.height / 2 - system.painting.height / 2
    paste(system.painting, x, y);

    const d = 32,
      s = rr(1, 4);
    page(system.painting)
      .ink()
      .box(pen.x + rr(-d, d), pen.y + rr(-d, d), s, "fill*center")
      .page(screen);
  }
}

// 📚 Library (Useful functions used throughout the piece)
// ...

export const system = "nopaint";
export { paint };
