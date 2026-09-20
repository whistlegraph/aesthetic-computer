// ruzomo, 26.09.13
// A blank Aesthetic Computer piece.

function paint({ wipe, ink, screen }) {
  wipe(70, 50, 100);
  ink(255, 100, 255).write("ruzomo", { center: "xy" });
  return false; // Painted once; return true to keep painting.
}

export { paint };
