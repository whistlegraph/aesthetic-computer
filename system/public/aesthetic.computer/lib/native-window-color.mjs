// A local-only bridge: three color channels, at most ten messages per second.
// A change after a quiet spell posts at once (the first frame's colour, a
// theme flip); changes inside the 100 ms window collapse into one trailing post.
export function createWindowColorReporter(post, schedule = setTimeout, now = () => performance.now()) {
  let pending, previous, scheduled = false, postedAt = -Infinity;
  const flush = () => {
    scheduled = false;
    const key = pending.join(",");
    if (key === previous) return;
    previous = key;
    postedAt = now();
    post({ type: "window:color", body: pending });
  };
  return (rgb) => {
    if (!Array.isArray(rgb) || rgb.length < 3 || !rgb.slice(0, 3).every(Number.isFinite)) return;
    pending = rgb.slice(0, 3).map(value => Math.max(0, Math.min(255, Math.round(value))));
    if (scheduled) return;
    const wait = 100 - (now() - postedAt);
    if (wait <= 0) return flush();
    scheduled = true;
    schedule(flush, wait);
  };
}
