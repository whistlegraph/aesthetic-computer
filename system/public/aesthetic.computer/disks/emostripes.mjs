// emostripes, 26.04.21

const { floor, sin, cos, abs, PI, random } = Math;

const count = 16;
let frame = 0;

// Harsh emo palette: blood, bruise, bile, bone, void
const palette = [
  [180, 0, 0],     // blood red
  [90, 0, 120],    // bruised purple
  [0, 120, 60],    // sickly green
  [10, 10, 10],    // near black
  [255, 255, 255], // harsh white
  [60, 0, 0],      // dried blood
  [0, 0, 80],      // cold blue-black
  [180, 160, 0],   // sick yellow
];

const offsets = Array.from({ length: count }, () => random() * 1000);
const glitchTimers = Array.from({ length: count }, () => 0);

function paint({ wipe, ink, screen: { width: w, height: h } }) {
  wipe(0);
  frame += 1;

  const baseW = w / count;

  for (let i = 0; i < count; i++) {
    const off = offsets[i];

    // Jagged width — stripes breathe unevenly
    const jag = sin(frame * 0.03 + off * 0.7) * 0.6 + cos(frame * 0.05 + off) * 0.4;
    const stripeW = floor(baseW * (0.4 + abs(jag) * 1.2)) + 1;

    // Horizontal drift
    const drift = sin(frame * 0.015 + off * 1.3) * baseW * 0.8;
    const x = floor(i * baseW + drift);

    const colorPhase = (frame * 0.008 + off * 0.4) % palette.length;
    const ci = floor(colorPhase) % palette.length;
    const [r, g, b] = palette[ci];

    const flicker = abs(sin(frame * 0.12 + off * 2.1));
    const harsh = floor(flicker * flicker * 255);

    glitchTimers[i] = (glitchTimers[i] + 1) % floor(40 + off % 80);
    const isGlitch = glitchTimers[i] < 2;

    if (isGlitch) {
      ink(255, 255, 255, 180).box(x, 0, stripeW, h, "fill");
    } else {
      const scanLines = 6;
      for (let s = 0; s < scanLines; s++) {
        const y0 = floor(s * h / scanLines);
        const y1 = floor((s + 1) * h / scanLines);
        const scanShift = floor(sin(frame * 0.07 + s * 1.4 + off) * 3);
        const alpha = 120 + floor(harsh * 0.5);
        ink(
          floor(r * (0.5 + flicker * 0.5)),
          floor(g * (0.5 + flicker * 0.5)),
          floor(b * (0.5 + flicker * 0.5)),
          alpha,
        ).box(x + scanShift, y0, stripeW, y1 - y0, "fill");
      }
    }
  }

  // Horizontal noise bars — VHS artifact
  for (let b = 0; b < 3; b++) {
    const barY = floor(((frame * (b + 1) * 7) % (h + 40)) - 20);
    const barH = 2 + floor(abs(sin(frame * 0.04 + b * 2.1)) * 4);
    ink(255, 255, 255, 25 + floor(abs(sin(frame * 0.1 + b)) * 40))
      .box(0, barY, w, barH, "fill");
  }
}

function meta() {
  return { title: "Emostripes", desc: "Harsh, glitchy, emo stripes." };
}

export { paint, meta };
