// Continuous warm DMX color, evaluated against a shared score clock.
// Output is RGB only; fixture routing and start/stop fades belong to the caller.
function noise(time, period, seed) {
  const cell = Math.floor(time / period);
  const part = time / period - cell;
  const smooth = part * part * (3 - 2 * part);
  const value = (index) => {
    const n = Math.sin(index * 127.1 + seed * 311.7) * 43758.5453;
    return (n - Math.floor(n)) * 2 - 1;
  };
  return value(cell) * (1 - smooth) + value(cell + 1) * smooth;
}

export function candlelightRgb(scoreTime, fixture = 0) {
  if (!Number.isFinite(scoreTime) || scoreTime < 0) return [0, 0, 0];
  const seed = Number.isFinite(fixture) ? fixture : 0;
  // Quick motion stays shallow; slower drift gives each flame its own shape.
  const shimmer = noise(scoreTime, 0.17, seed + 1);
  const drift = noise(scoreTime, 1.7, seed + 19);
  const warmth = noise(scoreTime, 0.43, seed + 43);
  const intensity = 0.90 + shimmer * 0.035 + drift * 0.045;
  return [
    Math.round(48 * intensity),
    Math.round((23 + warmth * 1.5) * intensity),
    Math.round(2 * intensity),
  ];
}
