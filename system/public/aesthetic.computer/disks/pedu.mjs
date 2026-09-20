// pedu, 26.09.13

function paint({ wipe, ink, screen }) {
  const { width: w, height: h } = screen;
  const pad = 16;
  const thick = max(8, floor(min(w, h) / 12));
  wipe(70, 50, 100);
  ink(255, 0, 0).line(pad, h - pad - thick, w - pad, pad + 24, thick);
  return false;
}

const { max, min, floor } = Math;

export { paint };
