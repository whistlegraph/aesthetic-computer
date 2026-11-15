// braincell-meter.mjs — how the braincell count on the status line moves.
//
// One step of the roll from the number drawn toward the number held: a twelfth
// of the gap, at least one. A fifth emptied most of a charge in half a second,
// which read as a jump; this takes about a second and counts the last cells singly.
export function rollToward(shown, held) {
  if (!Number.isFinite(shown) || !Number.isFinite(held)) return held;
  const gap = held - shown;
  if (Math.abs(gap) <= 1) return held;
  return shown + Math.sign(gap) * Math.max(1, Math.round(Math.abs(gap) / 12));
}
