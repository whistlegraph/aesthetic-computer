// selection — text chosen with the mouse, inside the interface.
//
// When the interface holds the terminal's mouse (so the bottom line can be
// clicked and hovered), the terminal no longer selects text on a drag. So the
// interface does: a press in the transcript anchors, a drag stretches, and the
// release puts the words on the clipboard. Everything here is geometry on
// plain rows — 1-based terminal columns and rows, the way the mouse reports
// them — so the frame that paints the highlight and the code that copies the
// text agree to the cell.

// Anchor and head in either order become a range that reads top-down.
export function normalize(selection) {
  if (!selection) return null;
  const [ax, ay] = selection.anchor;
  const [hx, hy] = selection.head;
  if (ay < hy || (ay === hy && ax <= hx)) return { x1: ax, y1: ay, x2: hx, y2: hy };
  return { x1: hx, y1: hy, x2: ax, y2: ay };
}

// The columns of one row that fall inside the range, or null: [from, to],
// inclusive, 1-based.
export function rowSpan(rowY, selection, width) {
  const range = normalize(selection);
  if (!range || rowY < range.y1 || rowY > range.y2) return null;
  const from = rowY === range.y1 ? range.x1 : 1;
  const to = rowY === range.y2 ? range.x2 : width;
  return from <= to ? [from, to] : null;
}

// The words under the range: each row's slice, right-trimmed, joined by
// newlines, with the one-cell margin the frame draws on the left removed.
export function selectedText(rows, selection, width, margin = 1) {
  const range = normalize(selection);
  if (!range) return "";
  const lines = [];
  for (let y = range.y1; y <= range.y2; y += 1) {
    const span = rowSpan(y, selection, width);
    const row = rows[y - 1] || "";
    if (!span) continue;
    const chars = Array.from(row);
    lines.push(chars.slice(Math.max(margin, span[0] - 1), span[1]).join("").replace(/\s+$/, ""));
  }
  // Blank rows at either end are padding, not words.
  return lines.join("\n").replace(/^\n+/, "").replace(/\n+$/, "");
}
