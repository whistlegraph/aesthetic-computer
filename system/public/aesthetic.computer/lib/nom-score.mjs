// Shared scoring rules for the Nom games.

export const NOM_SCORE_LIMIT = 1_000_000_000;

export function munchPoints(combo) {
  const streak = Math.max(1, Math.trunc(Number(combo) || 1));
  return 100 + (streak - 1) * 25;
}

export function clearPoints(level, beatsLeft) {
  const board = Math.max(1, Math.trunc(Number(level) || 1));
  const beats = Math.max(0, Math.trunc(Number(beatsLeft) || 0));
  return board * 250 + beats * 10;
}

export function normalizeNomRun(value = {}) {
  const score = Math.trunc(Number(value.score));
  const level = Math.trunc(Number(value.level));
  const correct = Math.trunc(Number(value.correct));
  if (
    !Number.isFinite(score) || score < 0 || score > NOM_SCORE_LIMIT ||
    !Number.isFinite(level) || level < 1 || level > 10_000 ||
    !Number.isFinite(correct) || correct < 0 || correct > 1_000_000
  ) return null;
  return { score, level, correct };
}

// Positive means a is the stronger run. Earlier achievement wins an exact tie.
export function compareNomRuns(a, b) {
  if (!b) return 1;
  if (a.score !== b.score) return a.score - b.score;
  if (a.level !== b.level) return a.level - b.level;
  if (a.correct !== b.correct) return a.correct - b.correct;
  const at = new Date(a.when || 0).getTime();
  const bt = new Date(b.when || 0).getTime();
  return bt - at;
}
