// Line, 25.09.28
// Minimal line brush compatible with robo gesture pipeline and manual nopaint usage.

import { setFadeAlpha } from "../lib/fade-state.mjs";

const activeStroke = [];
let strokeStartKey = null;

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

function sanitizeLinePoints(points) {
  if (!Array.isArray(points)) return [];
  const sanitized = [];
  for (const point of points) {
    if (!point) continue;
    if (Array.isArray(point) && point.length >= 2) {
      const [x, y] = point;
      if (Number.isFinite(x) && Number.isFinite(y)) {
        sanitized.push({ x, y });
      }
    } else if (typeof point.x === "number" && typeof point.y === "number") {
      sanitized.push({ x: point.x, y: point.y });
    }
  }
  return sanitized;
}

function roundPoints(points) {
  return points.map(({ x, y }) => ({ x: Math.round(x), y: Math.round(y) }));
}

function createPointsFromBox(box) {
  if (!box) return [];
  const { x, y } = box;
  const w = typeof box.w === "number" ? box.w : 0;
  const h = typeof box.h === "number" ? box.h : 0;

  if (!Number.isFinite(x) || !Number.isFinite(y)) {
    return [];
  }

  const endX = Number.isFinite(x + w) ? x + w : x;
  const endY = Number.isFinite(y + h) ? y + h : y;

  return sanitizeLinePoints([
    { x, y },
    { x: endX, y: endY }
  ]);
}

function ensureStrokeInitialized(system) {
  const np = system?.nopaint;
  if (!np?.startDrag) return;
  const key = `${np.startDrag.x},${np.startDrag.y}`;
  if (activeStroke.length === 0 || strokeStartKey !== key) {
    activeStroke.length = 0;
    strokeStartKey = key;
    activeStroke.push({ x: np.startDrag.x, y: np.startDrag.y });
  }
}

function paintingStrokeToScreen(points, system) {
  const np = system?.nopaint;
  if (!np) return [];
  const zoom = np.zoomLevel || 1;
  const tx = np.translation?.x || 0;
  const ty = np.translation?.y || 0;
  return points.map(({ x, y }) => ({
    x: Math.round(x * zoom + tx),
    y: Math.round(y * zoom + ty)
  }));
}

function appendStrokePoint(system) {
  const np = system?.nopaint;
  if (!np?.brush) return;
  const point = { x: np.brush.x, y: np.brush.y };
  const last = activeStroke[activeStroke.length - 1];
  if (!last || last.x !== point.x || last.y !== point.y) {
    activeStroke.push(point);
    if (activeStroke.length === 1 && strokeStartKey === null) {
      strokeStartKey = `${point.x},${point.y}`;
    }
  }
}

function collectLinePointsForOverlay({ mark, pen, system }) {
  const np = system?.nopaint;
  if (np?.startDrag && np?.brush) {
    const zoom = np.zoomLevel || 1;
    const tx = np.translation?.x || 0;
    const ty = np.translation?.y || 0;
    const start = {
      x: np.startDrag.x * zoom + tx,
      y: np.startDrag.y * zoom + ty
    };
    const current = {
      x: np.brush.x * zoom + tx,
      y: np.brush.y * zoom + ty
    };
    const points = sanitizeLinePoints([start, current]);
    if (points.length >= 2) {
      return points;
    }
  }

  if (mark?.points) {
    const points = sanitizeLinePoints(mark.points);
    if (points.length >= 2) return points;
  }

  if (Array.isArray(mark) && mark.length) {
    const points = sanitizeLinePoints(mark);
    if (points.length >= 2) return points;
  }

  if (pen?.points) {
    const points = sanitizeLinePoints(pen.points);
    if (points.length >= 2) return points;
  }

  if (pen?.dragBox) {
    const points = createPointsFromBox(pen.dragBox);
    if (points.length >= 2) return points;
  }

  const boxPoints = createPointsFromBox(mark);
  if (boxPoints.length >= 2) {
    return boxPoints;
  }

  return [];
}

function collectLinePointsForLift({ mark, pen, system }) {
  const np = system?.nopaint;
  if (np?.finalStartDrag && np?.finalDragBox) {
    const start = np.finalStartDrag;
    const drag = np.finalDragBox;
    const end = np.finalEndPoint
      ? { x: np.finalEndPoint.x, y: np.finalEndPoint.y }
      : {
          x: start.x + (typeof drag.w === "number" ? drag.w : 0),
          y: start.y + (typeof drag.h === "number" ? drag.h : 0)
        };
    const points = sanitizeLinePoints([start, end]);
    if (points.length >= 2) return points;
  }

  if (mark?.painting?.points) {
    const points = sanitizeLinePoints(mark.painting.points);
    if (points.length >= 2) return points;
  }

  if (mark?.points) {
    const points = sanitizeLinePoints(mark.points);
    if (points.length >= 2) return points;
  }

  if (Array.isArray(mark) && mark.length) {
    const points = sanitizeLinePoints(mark);
    if (points.length >= 2) return points;
  }

  if (pen?.points) {
    const points = sanitizeLinePoints(pen.points);
    if (points.length >= 2) return points;
  }

  if (pen?.dragBox) {
    const points = createPointsFromBox(pen.dragBox);
    if (points.length >= 2) return points;
  }

  const boxPoints = createPointsFromBox(mark);
  if (boxPoints.length >= 2) {
    return boxPoints;
  }

  return [];
}

function drawLine(drawer, points, thickness, antialias) {
  if (points.length < 2) return;

  const rounded = roundPoints(points);

  if (thickness <= 1) {
    if (antialias && typeof drawer.line === "function") {
      for (let i = 0; i < rounded.length - 1; i++) {
        const p1 = rounded[i];
        const p2 = rounded[i + 1];
        drawer.line(p1.x, p1.y, p2.x, p2.y, true);
      }
    } else if (typeof drawer.pppline === "function") {
      drawer.pppline(rounded);
    } else if (typeof drawer.line === "function") {
      for (let i = 0; i < rounded.length - 1; i++) {
        const p1 = rounded[i];
        const p2 = rounded[i + 1];
        drawer.line(p1.x, p1.y, p2.x, p2.y, false);
      }
    }
    return;
  }

  if (typeof drawer.pline === "function") {
    drawer.pline(rounded, thickness);
  } else if (typeof drawer.pppline === "function") {
    drawer.pppline(rounded);
  }
}

function overlay({ ink, color, mark, pen, system }) {
  if (!mark && !pen) return;
  const thickness = Math.max(1, Math.round(mark?.thickness ?? pen?.thickness ?? 1));
  const antialias = mark?.antialias ?? pen?.antialias ?? thickness <= 1;

  let points = [];
  const penDrawing = !!(pen && pen.drawing);

  if (penDrawing && system?.nopaint) {
    ensureStrokeInitialized(system);
    appendStrokePoint(system);
    points = paintingStrokeToScreen(activeStroke, system);
  } else {
    points = collectLinePointsForOverlay({ mark, pen, system });
    if (points.length < 2 && activeStroke.length >= 2) {
      points = paintingStrokeToScreen(activeStroke, system);
    }
  }

  if (points.length < 2) return;

  try {
    const drawer = resolveInk(ink, color);
    drawLine(drawer, points, thickness, antialias);
  } catch (error) {
    console.error("🖊️ LINE overlay error:", error);
  }
}

function lift({ ink, color, mark, pen, system }) {
  if (!mark && !pen) return false;
  const thickness = Math.max(1, Math.round(mark?.thickness ?? pen?.thickness ?? 1));
  const antialias = mark?.antialias ?? pen?.antialias ?? thickness <= 1;
  if (activeStroke.length >= 1 && system?.nopaint?.finalEndPoint) {
    const end = system.nopaint.finalEndPoint;
    const last = activeStroke[activeStroke.length - 1];
    if (last.x !== end.x || last.y !== end.y) {
      activeStroke.push({ x: end.x, y: end.y });
    }
  }
  const preferredPoints = activeStroke.length >= 2 ? [...activeStroke] : [];
  const points = preferredPoints.length >= 2 ? preferredPoints : collectLinePointsForLift({ mark, pen, system });
  if (points.length < 2) {
    console.warn("🖊️ LINE lift: not enough points to draw", points);
    activeStroke.length = 0;
    strokeStartKey = null;
    return false;
  }

  try {
    const drawer = resolveInk(ink, color);
    drawLine(drawer, points, thickness, antialias);
    activeStroke.length = 0;
    strokeStartKey = null;
    return true;
  } catch (error) {
    console.error("🖊️ LINE lift error:", error);
    activeStroke.length = 0;
    strokeStartKey = null;
    return false;
  }
}

export { overlay, lift };
