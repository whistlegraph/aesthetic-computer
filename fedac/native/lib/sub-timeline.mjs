// A continuous presentation clock: small network corrections change speed,
// never position. Start a new key when the conductor starts a new run.
export function createSubTimeline() {
  let anchor = null, local = 0, rate = 1, key;
  const at = now => anchor === null ? null : anchor + (now - local) * rate;
  return {
    at,
    update(scoreTime, now, run) {
      if (![scoreTime, now].every(Number.isFinite)) return false;
      if (anchor === null || key !== run) {
        anchor = scoreTime; local = now; rate = 1; key = run;
      } else {
        const predicted = at(now);
        const error = scoreTime - predicted;
        anchor = predicted; local = now;
        rate = 1 + Math.max(-0.02, Math.min(0.02, error / 2));
      }
      return true;
    },
  };
}
