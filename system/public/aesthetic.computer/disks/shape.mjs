// Shape, 25.09.28
// Simple filled shape brush compatible with robo gestures and nopaint overlay/lift flow.

import { setFadeAlpha } from "../lib/fade-state.mjs";

const activeShape = [];
let wasPenDrawing = false;

function normalizeColorSpec(colorSpec) {
  if (!colorSpec) return [255, 255, 255];
  if (Array.isArray(colorSpec)) return colorSpec;
  if (typeof colorSpec === "string") return colorSpec;
  if (typeof colorSpec === "object") {
    if (colorSpec.r !== undefined && colorSpec.g !== undefined && colorSpec.b !== undefined) {
      if (colorSpec.a !== undefined) {
        return [colorSpec.r, colorSpec.g, colorSpec.b, colorSpec.a];
      }
      return [colorSpec.r, colorSpec.g, colorSpec.b];
    }
    if (colorSpec.color) {
      return normalizeColorSpec(colorSpec.color);
    }
  }
  return colorSpec;
}

function resolveInk(ink, colorSpec) {
  const normalized = normalizeColorSpec(colorSpec);

  if (Array.isArray(normalized)) {
    if (normalized.length > 0 && typeof normalized[0] === "string" && normalized[0].startsWith("fade")) {
      if (typeof normalized[1] === "number") {
        setFadeAlpha(normalized[1]);
      }
      return ink(normalized[0]);
    }
    return ink(normalized);
  }

  if (typeof normalized === "string") {
    if (normalized.startsWith("fade")) {
      return ink(normalized);
    }
    return ink(normalized);
  }

  return ink([255, 255, 255]);
}

function isValidPoint(point) {
  return point && Number.isFinite(point.x) && Number.isFinite(point.y);
}

function sanitizeShapePoints(points, closed = true) {
  if (!Array.isArray(points)) return [];
  const sanitized = [];
  for (const point of points) {
    if (!point) continue;
    if (Array.isArray(point) && point.length >= 2) {
      const [x, y] = point;
      if (Number.isFinite(x) && Number.isFinite(y)) {
        sanitized.push([x, y]);
      }
    } else if (typeof point.x === "number" && typeof point.y === "number") {
      sanitized.push([point.x, point.y]);
    }
  }
  if (closed && sanitized.length >= 2) {
    const first = sanitized[0];
    const last = sanitized[sanitized.length - 1];
    if (first[0] !== last[0] || first[1] !== last[1]) {
      sanitized.push([first[0], first[1]]);
    }
  }
  return sanitized;
}

function roundShapePoints(points) {
  return points.map(([x, y]) => [Math.round(x), Math.round(y)]);
}

function screenToPainting(point, system) {
  if (!point) return null;
  const np = system?.nopaint;
  if (!np) return { x: point.x, y: point.y };
  const zoom = np.zoomLevel || 1;
  const tx = np.translation?.x || 0;
  const ty = np.translation?.y || 0;
  if (!zoom || !Number.isFinite(zoom)) return { x: point.x, y: point.y };
  const x = (point.x - tx) / zoom;
  const y = (point.y - ty) / zoom;
  if (!Number.isFinite(x) || !Number.isFinite(y)) return null;
  return { x, y };
}

function paintingShapeToScreen(points, system) {
  const np = system?.nopaint;
  const zoom = np?.zoomLevel || 1;
  const tx = np?.translation?.x || 0;
  const ty = np?.translation?.y || 0;
  return points.map(({ x, y }) => ({
    x: Math.round(x * zoom + tx),
    y: Math.round(y * zoom + ty)
  }));
}

function pushUniquePaintingPoint(point) {
  if (!isValidPoint(point)) return false;
  const last = activeShape[activeShape.length - 1];
  if (last && last.x === point.x && last.y === point.y) return false;
  activeShape.push({ x: point.x, y: point.y });
  return true;
}

function ensureShapeInitialized(system, pen) {
  const np = system?.nopaint;
  let start = null;
  if (np?.startDrag && isValidPoint(np.startDrag)) {
    start = { x: np.startDrag.x, y: np.startDrag.y };
  } else if (pen && Number.isFinite(pen.x) && Number.isFinite(pen.y)) {
    start = screenToPainting({ x: pen.x, y: pen.y }, system);
  }
  if (!isValidPoint(start)) return;
  if (activeShape.length === 0) {
    pushUniquePaintingPoint(start);
  }
}

function appendShapePoint(system, pen) {
  const np = system?.nopaint;
  let point = null;
  if (np?.brush && isValidPoint(np.brush)) {
    point = { x: np.brush.x, y: np.brush.y };
  } else if (pen && Number.isFinite(pen.x) && Number.isFinite(pen.y)) {
    point = screenToPainting({ x: pen.x, y: pen.y }, system);
  }
  if (!point) return;
  pushUniquePaintingPoint(point);
}

function ensureFinalPoint(system, pen) {
  const np = system?.nopaint;
  if (np?.finalEndPoint && isValidPoint(np.finalEndPoint)) {
    pushUniquePaintingPoint(np.finalEndPoint);
  } else if (pen && Number.isFinite(pen.x) && Number.isFinite(pen.y)) {
    const projected = screenToPainting({ x: pen.x, y: pen.y }, system);
    pushUniquePaintingPoint(projected);
  }
}

function activeShapeToArray(close = true) {
  if (activeShape.length === 0) return [];
  const arr = activeShape.map(({ x, y }) => [x, y]);
  if (close && arr.length >= 2) {
    const first = arr[0];
    const last = arr[arr.length - 1];
    if (first[0] !== last[0] || first[1] !== last[1]) {
      arr.push([first[0], first[1]]);
    }
  }
  return arr;
}

function ensureClosed(points, closed = true) {
  if (!closed || points.length < 2) return points;
  const first = points[0];
  const last = points[points.length - 1];
  if (first[0] === last[0] && first[1] === last[1]) return points;
  return [...points, [first[0], first[1]]];
}

function overlay({ ink, color, mark, pen, system }) {
  if (!mark && !pen && activeShape.length === 0) return;
  const np = system?.nopaint;
  const penDrawing = !!(pen && pen.drawing);
  const paintingActive = penDrawing || (!!np?.startDrag && !!np?.brush);
  const outline = mark?.outline ?? false;
  const closed = mark?.closed ?? true;

  let points = [];

  if (paintingActive && system?.nopaint) {
    ensureShapeInitialized(system, pen);
    appendShapePoint(system, pen);
    const screenPoints = paintingShapeToScreen(activeShape, system);
    points = ensureClosed(screenPoints.map(({ x, y }) => [x, y]), closed);
  } else if (mark?.points || Array.isArray(mark)) {
    const source = mark?.points ?? mark;
    points = ensureClosed(sanitizeShapePoints(source, closed), closed);
    if (points.length < 3 && activeShape.length >= 3) {
      const screenPoints = paintingShapeToScreen(activeShape, system);
      points = ensureClosed(screenPoints.map(({ x, y }) => [x, y]), closed);
    }
  } else if (pen?.points) {
    points = ensureClosed(sanitizeShapePoints(pen.points, closed), closed);
    if (points.length < 3 && activeShape.length >= 3) {
      const screenPoints = paintingShapeToScreen(activeShape, system);
      points = ensureClosed(screenPoints.map(({ x, y }) => [x, y]), closed);
    }
  } else if (activeShape.length >= 3) {
    const screenPoints = paintingShapeToScreen(activeShape, system);
    points = ensureClosed(screenPoints.map(({ x, y }) => [x, y]), closed);
  }

  if (!paintingActive && wasPenDrawing) {
    ensureFinalPoint(system, pen);
    if (points.length < 3 && activeShape.length >= 3) {
      const screenPoints = paintingShapeToScreen(activeShape, system);
      points = ensureClosed(screenPoints.map(({ x, y }) => [x, y]), closed);
    }
  }

  if (points.length < 3) return;

  try {
    const drawer = resolveInk(ink, color);
    const rounded = roundShapePoints(points);
    if (outline) {
      if (typeof drawer.pppline === "function") {
        drawer.pppline(rounded.map(([x, y]) => ({ x, y })));
      } else if (typeof drawer.line === "function") {
        for (let i = 0; i < rounded.length - 1; i++) {
          const [x1, y1] = rounded[i];
          const [x2, y2] = rounded[i + 1];
          drawer.line(x1, y1, x2, y2, true);
        }
      }
    } else if (typeof drawer.shape === "function") {
      drawer.shape(rounded);
    }
  } catch (error) {
    console.error("🔷 SHAPE overlay error:", error);
  }

  wasPenDrawing = paintingActive;
}

function lift({ ink, color, mark, pen, system }) {
  if (!mark && activeShape.length < 3) {
    return false;
  }

  if (activeShape.length >= 1) {
    ensureFinalPoint(system, pen);
  }

  const outline = mark?.outline ?? false;
  const closed = mark?.closed ?? true;
  const preferred = activeShape.length >= 3 ? activeShapeToArray(closed) : [];
  const fallbackSource = mark?.points ?? (Array.isArray(mark) ? mark : []);
  const points = preferred.length >= 3
    ? preferred
    : ensureClosed(sanitizeShapePoints(fallbackSource, closed), closed);

  if (points.length < 3) {
    console.warn("🔷 SHAPE lift: not enough points to draw", points);
    activeShape.length = 0;
    wasPenDrawing = false;
    return false;
  }

  try {
    const drawer = resolveInk(ink, color);
    const rounded = roundShapePoints(points);
    if (outline) {
      if (typeof drawer.pppline === "function") {
        drawer.pppline(rounded.map(([x, y]) => ({ x, y })));
      } else if (typeof drawer.line === "function") {
        for (let i = 0; i < rounded.length - 1; i++) {
          const [x1, y1] = rounded[i];
          const [x2, y2] = rounded[i + 1];
          drawer.line(x1, y1, x2, y2, true);
        }
      }
    } else if (typeof drawer.shape === "function") {
      drawer.shape(rounded);
    }
    activeShape.length = 0;
    wasPenDrawing = false;
    return true;
  } catch (error) {
    console.error("🔷 SHAPE lift error:", error);
    activeShape.length = 0;
    wasPenDrawing = false;
    return false;
  }
}

export const system = "nopaint";

export { overlay, lift };
