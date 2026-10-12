// A local-only bridge: three color channels, at most ten messages per second.
export function createWindowColorReporter(post, schedule = setTimeout) {
  let pending, previous, scheduled = false;
  return (rgb) => {
    if (!Array.isArray(rgb) || rgb.length < 3 || !rgb.slice(0, 3).every(Number.isFinite)) return;
    pending = rgb.slice(0, 3).map(value => Math.max(0, Math.min(255, Math.round(value))));
    if (scheduled) return;
    scheduled = true;
    schedule(() => {
      scheduled = false;
      const key = pending.join(",");
      if (key === previous) return;
      previous = key;
      post({ type: "window:color", body: pending });
    }, 100);
  };
}
