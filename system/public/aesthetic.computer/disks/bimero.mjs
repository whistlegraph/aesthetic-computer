// bimero, 26.09.13
// A blank Aesthetic Computer piece.

function paint({ wipe, ink, screen }) {
  wipe(30, 90, 120);
  ink(255, 100, 255).write("bimero", { center: "xy" });
  return false; // Painted once; return true to keep painting.
}

export { paint };
