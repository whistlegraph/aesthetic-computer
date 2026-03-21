// Rainbow X
// Draws a centered rainbow X with extra geometric accents.

const BACKGROUND = [6, 8, 12];
const LINE_SEGMENTS = 48;
const RING_SEGMENTS = 72;
const TAU = Math.PI * 2;

function drawRainbowLine({ ink, line, num }, x0, y0, x1, y1, hueOffset) {
  const dx = x1 - x0;
  const dy = y1 - y0;

  for (let i = 0; i < LINE_SEGMENTS; i += 1) {
    const t0 = i / LINE_SEGMENTS;
    const t1 = (i + 1) / LINE_SEGMENTS;
    const hue = (t0 * 360 + hueOffset) % 360;
    const color = num.hslToRgb(hue, 100, 55);

    ink(...color);
    line(x0 + dx * t0, y0 + dy * t0, x0 + dx * t1, y0 + dy * t1);
  }
}

function drawRainbowRing({ ink, line, num }, cx, cy, radius, hueOffset) {
  for (let i = 0; i < RING_SEGMENTS; i += 1) {
    const t0 = i / RING_SEGMENTS;
    const t1 = (i + 1) / RING_SEGMENTS;
    const angle0 = t0 * TAU;
    const angle1 = t1 * TAU;
    const hue = (t0 * 360 + hueOffset) % 360;
    const color = num.hslToRgb(hue, 100, 55);

    ink(...color);
    line(
      cx + Math.cos(angle0) * radius,
      cy + Math.sin(angle0) * radius,
      cx + Math.cos(angle1) * radius,
      cy + Math.sin(angle1) * radius,
    );
  }
}

function drawDiamond({ ink, line }, cx, cy, size, color) {
  const half = size / 2;

  ink(...color);
  line(cx, cy - half, cx + half, cy);
  line(cx + half, cy, cx, cy + half);
  line(cx, cy + half, cx - half, cy);
  line(cx - half, cy, cx, cy - half);
}

function paint({ wipe, ink, line, box, circle, screen, num }) {
  wipe(...BACKGROUND);

  const size = Math.min(screen.width, screen.height) * 0.7;
  const half = size / 2;
  const cx = screen.width / 2;
  const cy = screen.height / 2;
  const ringRadius = half * 0.9;
  const squareSize = size * 0.78;
  const diamondSize = size * 0.45;
  const dotRadius = Math.max(2, Math.round(size * 0.02));

  drawRainbowLine({ ink, line, num }, cx - half, cy - half, cx + half, cy + half, 0);
  drawRainbowLine({ ink, line, num }, cx - half, cy + half, cx + half, cy - half, 180);

  drawRainbowRing({ ink, line, num }, cx, cy, ringRadius, 90);
  drawRainbowRing({ ink, line, num }, cx, cy, ringRadius - 1, 270);

  ink(...num.hslToRgb(200, 70, 55));
  box(cx, cy, squareSize, squareSize, "outline:2*center");

  drawDiamond({ ink, line }, cx, cy, diamondSize, num.hslToRgb(320, 70, 58));

  ink(...num.hslToRgb(45, 90, 60));
  circle(cx, cy, dotRadius, true);
}

export { paint };
