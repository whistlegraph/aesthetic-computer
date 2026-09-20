// neme, 26.09.13

const w = 24; // stripe width
let shift = 0;

function paint({ wipe, ink, screen, pen, num: { hslToRgb } }) {
  // the pen slides the pair around the wheel; red & blue are just where it rests.
  const hue = pen ? (pen.x / screen.width) * 360 : 0;
  const light = pen ? 65 - (pen.y / screen.height) * 30 : 50;
  const a = hslToRgb(hue, 75, light);
  const b = hslToRgb((hue + 240) % 360, 75, light);

  wipe(16, 16, 24);
  for (let x = -w * 2 + (shift % (w * 2)); x < screen.width; x += w * 2) {
    ink(a).box(x, 0, w, screen.height);
    ink(b).box(x + w, 0, w, screen.height);
  }
}

function sim() {
  shift += 1;
}

export { paint, sim };
