// balozo, 26.09.11
// A pink line that follows the pointer.

let pointer; // { x, y } — last known pen/mouse position.

function paint({ wipe, ink, screen, pen }) {
  const cx = screen.width / 2;
  const cy = screen.height / 2;
  const p = pointer || pen || { x: cx, y: cy };

  wipe(70, 50, 100);
  ink(255, 100, 255).line(cx, cy, p.x, p.y);
  ink(255, 100, 255).write("balozo", { center: "xy" });
}

function act({ event: e }) {
  if (e.is("move") || e.is("draw") || e.is("touch")) pointer = { x: e.x, y: e.y };
}

export { paint, act };
