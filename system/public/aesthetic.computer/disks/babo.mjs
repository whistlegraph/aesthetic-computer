// babo, 26.09.15
// A blank Aesthetic Computer piece.

function paint({ wipe }) {
  wipe(70, 50, 100);
  return false; // Painted once; return true to keep painting.
}

export { paint };
