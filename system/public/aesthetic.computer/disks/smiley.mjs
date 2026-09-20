// Smiley, 26.09.08.21.00
// A sunny face that follows your pointer and changes colour when tapped.

const palettes = [
  { bg: [255, 105, 180], face: [255, 226, 59], shade: [225, 153, 32] },
  { bg: [86, 205, 255], face: [255, 137, 70], shade: [217, 71, 71] },
  { bg: [139, 102, 241], face: [129, 255, 175], shade: [40, 184, 145] },
  { bg: [255, 218, 91], face: [255, 121, 190], shade: [211, 55, 145] },
];

let frame = 0;
let palette = 0;
let pop = 0;

function boot({ cursor }) {
  frame = 0;
  palette = 0;
  pop = 0;
  cursor("none");
}

function sim() {
  frame += 1;
  pop *= 0.9;
}

function paint({ ink, pointer, screen, wipe }) {
  const colors = palettes[palette];
  const cx = screen.width / 2;
  const cy = screen.height / 2 - Math.sin(frame * 0.035) * 3;
  const radius = Math.max(28, Math.min(screen.width, screen.height) * 0.34);
  const scale = 1 + pop * 0.08;
  const r = radius * scale;

  wipe(...colors.bg);

  // A small offset shadow keeps the face readable on every palette.
  ink(...colors.shade).circle(cx + r * 0.055, cy + r * 0.075, r, "fill");
  ink(...colors.face).circle(cx, cy, r, "fill");

  const px = pointer?.x ?? cx;
  const py = pointer?.y ?? cy;
  const dx = px - cx;
  const dy = py - cy;
  const distance = Math.hypot(dx, dy) || 1;
  const gaze = Math.min(r * 0.07, distance * 0.08);
  const gazeX = (dx / distance) * gaze;
  const gazeY = (dy / distance) * gaze;
  const eyeY = cy - r * 0.23;
  const eyeGap = r * 0.34;
  const blinking = frame % 300 > 286;

  if (blinking) {
    ink(35, 28, 42).box(cx - eyeGap - r * 0.12, eyeY, r * 0.24, r * 0.045);
    ink(35, 28, 42).box(cx + eyeGap - r * 0.12, eyeY, r * 0.24, r * 0.045);
  } else {
    ink(255).circle(cx - eyeGap, eyeY, r * 0.145, "fill");
    ink(255).circle(cx + eyeGap, eyeY, r * 0.145, "fill");
    ink(35, 28, 42).circle(
      cx - eyeGap + gazeX,
      eyeY + gazeY,
      r * 0.072,
      "fill",
    );
    ink(35, 28, 42).circle(
      cx + eyeGap + gazeX,
      eyeY + gazeY,
      r * 0.072,
      "fill",
    );
  }

  // Overlapping dots form a scale-independent smile without relying on arcs.
  const mouthY = cy + r * (0.16 + pop * 0.025);
  const smileDepth = r * (0.25 + pop * 0.08);
  const dotRadius = r * (0.045 + pop * 0.012);
  const mouth = ink(35, 28, 42);
  for (let i = 0; i <= 24; i += 1) {
    const u = i / 12 - 1;
    const x = cx + u * r * 0.47;
    const y = mouthY + (1 - u * u) * smileDepth;
    mouth.circle(x, y, dotRadius, "fill");
  }

  ink(255, 95, 120, 150).circle(cx - r * 0.61, cy + r * 0.13, r * 0.1, "fill");
  ink(255, 95, 120, 150).circle(cx + r * 0.61, cy + r * 0.13, r * 0.1, "fill");
}

function act({ event: e, needsPaint }) {
  if (
    e.is("touch") ||
    e.is("keyboard:down:space") ||
    e.is("keyboard:down:enter")
  ) {
    palette = (palette + 1) % palettes.length;
    pop = 1;
    needsPaint();
  }
}

function meta() {
  return {
    title: "Smiley",
    desc: "A sunny face that follows your pointer and changes colour when tapped.",
  };
}

export { act, boot, meta, paint, sim };
export const nohud = true;
