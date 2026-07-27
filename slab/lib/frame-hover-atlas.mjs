// Pure geometry for Frame's no-click hover exploration.

const clamp = (value, low, high) => Math.max(low, Math.min(high, value));

function boundsOf(env) {
  const crop = env?.crop;
  if (crop && [crop.x, crop.y, crop.w, crop.h].every(Number.isFinite)) {
    return [crop.x, crop.y, crop.w, crop.h];
  }
  const screen = env?.meta?.screen;
  if (screen && [screen.w, screen.h].every(Number.isFinite)) {
    return [0, 0, screen.w, screen.h];
  }
  throw new Error("Frame hover exploration needs capture bounds");
}

function inside([bx, by, bw, bh], x, y) {
  return x >= bx && x <= bx + bw && y >= by && y <= by + bh;
}

function unique(points, bounds, limit) {
  const seen = new Set();
  const result = [];
  for (const point of points) {
    const x = Math.round(Number(point.x));
    const y = Math.round(Number(point.y));
    if (!Number.isFinite(x) || !Number.isFinite(y) || !inside(bounds, x, y)) continue;
    const key = `${Math.round(x / 8)},${Math.round(y / 8)}`;
    if (seen.has(key)) continue;
    seen.add(key);
    result.push({ ...point, x, y });
    if (result.length >= limit) break;
  }
  return result;
}

export function buildHoverProbes(env, {
  mode = "wanderer", x, y, radius = 18, steps = mode === "wiggler" ? 9 : 14,
} = {}) {
  const bounds = boundsOf(env);
  const limit = clamp(Math.round(Number(steps) || 1), 1, 24);
  if (mode === "wiggler") {
    x = Number(x); y = Number(y); radius = clamp(Number(radius) || 18, 4, 120);
    if (!Number.isFinite(x) || !Number.isFinite(y)) {
      throw new Error("wiggler needs finite x and y coordinates");
    }
    const ring = [
      [0, 0], [-1, 0], [1, 0], [0, -1], [0, 1],
      [-0.72, -0.72], [0.72, -0.72], [0.72, 0.72], [-0.72, 0.72],
      [-0.45, 0], [0.45, 0], [0, -0.45], [0, 0.45],
    ];
    return unique(ring.map(([dx, dy], index) => ({
      x:x + dx * radius, y:y + dy * radius,
      kind:index === 0 ? "probe-center" : "hover-boundary",
      possibility:"hover boundary or cursor-shape change",
    })), bounds, limit);
  }

  if (mode !== "wanderer") throw new Error("mode must be wanderer or wiggler");
  const [bx, by, bw, bh] = bounds;
  const inset = Math.max(2, Math.min(6, Math.round(Math.min(bw, bh) * 0.004)));
  const points = [];

  // Semantic controls first, followed by image-discovered compact controls.
  for (const element of env?.ax?.elements || []) {
    const actions = element.actions || [];
    if (!actions.length && !/button|link|menu|control/i.test(element.role || "")) continue;
    points.push({
      x:element.cx, y:element.cy, kind:`ax-${element.role || "control"}`,
      label:String(element.title || "").replace(/\s+/g, " ").trim().slice(0, 80),
      possibility:actions.includes("AXPress") ? "button or pressable control" : "interactive control",
    });
  }
  // Focused-window geometry provides useful probes even when AX is sparse.
  points.push(
    { x:bx + bw / 2, y:by + Math.min(24, bh * 0.035), kind:"window-title", possibility:"drag window" },
    { x:bx + inset, y:by + bh / 2, kind:"window-left-edge", possibility:"resize window horizontally" },
    { x:bx + bw - inset, y:by + bh / 2, kind:"window-right-edge", possibility:"resize window horizontally" },
    { x:bx + bw / 2, y:by + bh - inset, kind:"window-bottom-edge", possibility:"resize window vertically" },
    { x:bx + inset, y:by + bh - inset, kind:"window-bottom-left-corner", possibility:"resize window diagonally" },
    { x:bx + bw - inset, y:by + bh - inset, kind:"window-bottom-right-corner", possibility:"resize window diagonally" },
  );
  for (const control of env?.visual || []) {
    points.push({
      x:control.cx, y:control.cy, kind:control.kind || "visual-control",
      possibility:"potential button or hover-only control",
    });
  }
  return unique(points, bounds, limit);
}

export function changesNearPoint(env, x, y, radius = 150) {
  const changes = (env?.diff || []).filter((change) => {
    const rect = Array.isArray(change.r) ? change.r.map(Number) : null;
    if (!rect || rect.length !== 4 || !rect.every(Number.isFinite)) return false;
    const cx = rect[0] + rect[2] / 2;
    const cy = rect[1] + rect[3] / 2;
    return Math.hypot(cx - x, cy - y) <= radius;
  });
  return {
    count:changes.length,
    cells:changes.reduce((sum, change) => sum + Math.max(1, Number(change.cells) || 1), 0),
  };
}
