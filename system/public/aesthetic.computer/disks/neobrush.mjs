function paint({ pen, ink }) {
  // Preview the box outline while dragging
  ink().box(pen.dragBox);  // ink().circle(pen.x, pen.y, 16);
}

function brush({ pen, ink, lift }) {
  if (lift) ink(255, 0, 0, 200).box(pen.dragBox, "outline");
}