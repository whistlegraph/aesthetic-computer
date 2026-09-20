// fugiza, 26.09.10
// A blank Aesthetic Computer piece.

function paint({ wipe, ink, screen }) {
  wipe(70, 50, 100);
  ink(255, 100, 255).write("fugiza", { center: "xy" });
  return false; // Painted once; return true to keep painting.
}

export { paint };
