// Rasterize the shared AC letters once, then scale their pixels and strokes
// together. Both buttons use the same whole-pixel scale.
const buttonLabelBitmaps = new Map();

function buttonLabelBitmap($, label) {
  const key = `${$.typeface.name}:${label}`;
  if (!buttonLabelBitmaps.has(key)) {
    if (![...label].every((letter) => (letter.trim() === "" || $.typeface.glyphs[letter]))) return null;
    buttonLabelBitmaps.set(key, $.painting(
      $.text.width(label) + 2, $.typeface.blockHeight + 2,
      (p) => p.wipe(0, 0, 0, 0).ink(255).write(label, { x: 1, y: 1 }),
    ));
  }
  return buttonLabelBitmaps.get(key);
}

export function buttonLabelSize($, button, label) {
  return Math.max(1, Math.floor(Math.min(
    button.box.h * 0.72 / ($.typeface.blockHeight + 2),
    button.box.w * 0.94 / ($.text.width(label) + 2),
  )));
}

export function paintDecisionButton($, button, label, flavor = "no", labelSize) {
  const active = button.down || button.over;
  const fill = flavor === "paint" || flavor === "done"
    ? active ? [18, 103, 46] : [26, 127, 58]
    : flavor === "back"
      ? active ? [146, 62, 6] : [175, 78, 10]
      : active ? [155, 20, 34] : [185, 30, 43];
  $.ink(fill)
    .box(button.box, "fill")
    .ink(255)
    .box(button.box, "outline");
  const bitmap = buttonLabelBitmap($, label);
  if (!bitmap) return;
  $.paste(bitmap,
    Math.round(button.box.x + (button.box.w - bitmap.width * labelSize) / 2),
    Math.round(button.box.y + (button.box.h - bitmap.height * labelSize) / 2),
    labelSize);
}

