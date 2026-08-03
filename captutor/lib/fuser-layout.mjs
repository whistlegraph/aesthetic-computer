// Deterministic, DOM-geometry-based packing for a live Fuser canvas.
// It plans screen-space node drags only; graph data and viewport stay untouched.

const DEFAULT_INSETS = Object.freeze({ left:96, top:88, right:88, bottom:88 });

export function planFuserRectPack(nodes, canvas, {
  insets = DEFAULT_INSETS,
  gapX = 56,
  gapY = 56,
} = {}) {
  if (!canvas || !Number.isFinite(canvas.width) || !Number.isFinite(canvas.height)) {
    throw new TypeError("planFuserRectPack needs a measured canvas rectangle");
  }
  const measured = nodes.filter((node) => node?.id && node.rect?.width > 0 && node.rect?.height > 0)
    .sort((a, b) => a.rect.y - b.rect.y || a.rect.x - b.rect.x || a.id.localeCompare(b.id));
  const left = canvas.x + insets.left;
  const top = canvas.y + insets.top;
  const right = canvas.x + canvas.width - insets.right;
  const bottom = canvas.y + canvas.height - insets.bottom;
  if (right <= left || bottom <= top) throw new Error("packing insets consume the canvas");

  const rows = [];
  let row = { nodes:[], width:0, height:0 };
  for (const node of measured) {
    const nextWidth = row.nodes.length ? row.width + gapX + node.rect.width : node.rect.width;
    if (row.nodes.length && nextWidth > right - left) {
      rows.push(row);
      row = { nodes:[], width:0, height:0 };
    }
    row.nodes.push(node);
    row.width = row.nodes.length === 1 ? node.rect.width : row.width + gapX + node.rect.width;
    row.height = Math.max(row.height, node.rect.height);
  }
  if (row.nodes.length) rows.push(row);

  const totalHeight = rows.reduce((sum, item) => sum + item.height, 0) + gapY * Math.max(0, rows.length - 1);
  let y = top + Math.max(0, (bottom - top - totalHeight) / 2);
  const placements = [];
  for (const item of rows) {
    let x = left + Math.max(0, (right - left - item.width) / 2);
    for (const node of item.nodes) {
      const target = { x, y:y + (item.height - node.rect.height) / 2, width:node.rect.width, height:node.rect.height };
      placements.push({
        nodeId:node.id,
        nodeType:node.type || null,
        from:{ x:node.rect.x, y:node.rect.y },
        target,
        delta:{ x:target.x - node.rect.x, y:target.y - node.rect.y },
      });
      x += node.rect.width + gapX;
    }
    y += item.height + gapY;
  }

  const overflow = Math.max(0, y - gapY - bottom);
  return {
    schema:"captutor-fuser-layout-plan/v1",
    mode:"rect-pack",
    canvas:{ x:canvas.x, y:canvas.y, width:canvas.width, height:canvas.height },
    insets, gaps:{ x:gapX, y:gapY }, rows:rows.length,
    fits:overflow === 0, overflow, placements,
  };
}
export function validateFuserLayoutPlan(plan) {
  const issues = [];
  const boxes = plan.placements.map((placement) => ({ id:placement.nodeId, ...placement.target }));
  for (let i = 0; i < boxes.length; i += 1) {
    for (let j = i + 1; j < boxes.length; j += 1) {
      const a = boxes[i], b = boxes[j];
      if (a.x < b.x + b.width && a.x + a.width > b.x && a.y < b.y + b.height && a.y + a.height > b.y) {
        issues.push(`overlap:${a.id}:${b.id}`);
      }
    }
  }
  if (!plan.fits) issues.push(`overflow:${plan.overflow}`);
  return { pass:issues.length === 0, issues };
}
