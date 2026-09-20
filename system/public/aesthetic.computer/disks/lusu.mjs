// lusu, 26.09.13
// Red, with stripes the pointer steers — x drifts them, y sets their width.

let shift = 0;
let pointer; // last known pen position, null until it moves
let held = false;

function paint({ wipe, ink, screen }) {
  wipe(180, 20, 30);

  const half = screen.width / 2;
  const w = pointer ? 4 + (pointer.y / screen.height) * 28 : 12;
  const gap = w * 2;

  for (let x = -gap + (((shift % gap) + gap) % gap); x < screen.width; x += gap) {
    ink(220, 60, 70).box(x, 0, w, screen.height);
  }

  if (held && pointer) {
    ink(255, 230, 220, 90).box(pointer.x - 20, 0, 40, screen.height);
  }

  ink(255, 220, 210).write("lusu", { center: "xy" });

  // drift speed rides on how far the pointer sits from center.
  shift += pointer ? ((pointer.x - half) / half) * 3 : 0.4;
}

function act({ event: e }) {
  if (e.is("move") || e.is("draw") || e.is("touch")) pointer = { x: e.x, y: e.y };
  if (e.is("touch")) held = true;
  if (e.is("lift")) held = false;
}

export { paint, act };
