// A rate-controlled view of network time. Wall time, auth, and network resync stay untouched.
// Keep musical phase continuous when the rate changes; reset explicitly rejoins UTC.
export function createPlaybackClock(networkNow) {
  let rate = 1, anchor = null, musical = 0;
  const at = now => anchor === null ? now : musical + (now - anchor) * rate;
  return {
    time: () => at(networkNow()),
    get rate() { return rate; },
    setRate(value, reset = false) {
      if (!Number.isFinite(value) || value < 0.25 || value > 2) return false;
      if (reset) { rate = value; anchor = value === 1 ? null : networkNow(); musical = anchor ?? 0; }
      else if (value !== rate) {
        const now = networkNow();
        musical = at(now); anchor = now; rate = value;
      }
      return true;
    },
  };
}
