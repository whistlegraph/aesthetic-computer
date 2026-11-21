// Frame intervals in presentation order. Both renderers sample only their own paints.
const columns = 56, width = 74;
const green = 0x77dda2, amber = 0xffcf69, red = 0xff8580;
const frameInk = ms => ms <= 18 ? green : ms <= 34 ? amber : red;

export function createFrameChart() {
  const gaps = new Float32Array(columns);
  let head = 0, count = 0, previous = null, elapsed = 0;
  function reset() {
    head = count = elapsed = 0;
    previous = null;
  }
  function sample(now) {
    if (previous !== null && now > previous) {
      const gap = now - previous;
      if (count === columns) elapsed -= gaps[head];
      gaps[head] = gap;
      elapsed += gaps[head];
      head = (head + 1) % columns;
      count = Math.min(columns, count + 1);
    }
    previous = now;
  }
  function draw({ box, text }, rows = []) {
    const graphY = 13, graphH = 12, graphX = 3;
    box(0, 0, width, 28 + rows.length * 9, 0x131a24, 0);
    const mean = count ? elapsed / count : 0;
    text(mean ? Math.round(1000 / mean) + ' fps' : '– fps', 3, 2, mean ? frameInk(mean) : 0xb5c2d1);
    box(graphX, graphY, columns, graphH, 0x243245, 1);
    const height = gap => Math.min(graphH, Math.max(1, Math.round(gap / 50 * graphH)));
    for (let i = 0; i < count; i++) {
      const gap = gaps[(head + columns - count + i) % columns], h = height(gap);
      box(graphX + columns - count + i, graphY + graphH - h, 1, h, frameInk(gap), 2);
    }
    // The reference tick is outside the graph so it never hides a short frame.
    const y = graphY + graphH - height(1000 / 60);
    box(graphX + columns, y, 2, 1, 0xb5c2d1, 2);
    text('60', graphX + columns + 3, y - 3, 0xb5c2d1);
    rows.forEach((row, i) => text(row, 3, 27 + i * 9, 0xc5d5e5));
  }
  return { sample, reset, draw, width };
}
