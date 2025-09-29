// Line, 25.09.28
// Minimal line brush compatible with robo gesture pipeline and manual nopaint usage.

import { setFadeAlpha } from "../lib/fade-state.mjs";

const activeStroke = [];
let strokeStartKey = null;
let wasPenDrawing = false;
let debugLineLogs = true;

function lineDebugEnabled() {
  return debugLineLogs === true;
}

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

function clonePoints(points) {
  if (!Array.isArray(points)) return [];
  return points.map(({ x, y, pressure }) => ({ x, y, pressure }));
}

function smoothStroke(rawPoints) {
  if (!Array.isArray(rawPoints) || rawPoints.length < 3) {
    return clonePoints(rawPoints);
  }

  const smoothed = [{ ...rawPoints[0] }];

  for (let i = 1; i < rawPoints.length - 1; i++) {
    const prev = rawPoints[i - 1];
    const curr = rawPoints[i];
    const next = rawPoints[i + 1];

    const dx1 = curr.x - prev.x;
    const dy1 = curr.y - prev.y;
    const dx2 = next.x - curr.x;
    const dy2 = next.y - curr.y;

    const vel1 = Math.hypot(dx1, dy1);
    const vel2 = Math.hypot(dx2, dy2);

    const dot = dx1 * dx2 + dy1 * dy2;
    const denom = vel1 * vel2;
    const cosAngle = denom > 0 ? dot / denom : 1;

    const directionChange = 1 - cosAngle;
    const velocityChange = Math.abs(vel1 - vel2);
    const isJitter = vel1 < 2 && vel2 < 2;

    let smoothingStrength = 0.25;
    if (isJitter) smoothingStrength = 0.6;
    else if (directionChange > 0.3) smoothingStrength = 0.4;
    else if (velocityChange > 5) smoothingStrength = 0.35;

    const averagedX = curr.x * (1 - smoothingStrength) + (prev.x + next.x) * (smoothingStrength / 2);
    const averagedY = curr.y * (1 - smoothingStrength) + (prev.y + next.y) * (smoothingStrength / 2);

    smoothed.push({
      x: averagedX,
      y: averagedY,
      pressure: curr.pressure
    });
  }

  smoothed.push({ ...rawPoints[rawPoints.length - 1] });
  return smoothed;
}

function prepareStroke(points, thickness, antialias) {
  if (!Array.isArray(points)) return [];
  if (points.length < 3) return clonePoints(points);

  // For now apply the same adaptive smoothing regardless of thickness/antialias.
  return smoothStroke(points);
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

function isValidPoint(point) {
  return point && Number.isFinite(point.x) && Number.isFinite(point.y);
}

function pushUniquePaintingPoint(point) {
  if (!isValidPoint(point)) return false;
  const last = activeStroke[activeStroke.length - 1];
  if (last && last.x === point.x && last.y === point.y) {
    return false;
  }
  activeStroke.push({ x: point.x, y: point.y });
  if (activeStroke.length === 1) {
    strokeStartKey = `${point.x},${point.y}`;
  }
  if (lineDebugEnabled()) {
    console.log("🟢 LINE pushUniquePaintingPoint", {
      point,
      length: activeStroke.length,
      strokeStartKey,
      timestamp: performance.now()
    });
  }
  return true;
}

function ensureStrokeInitialized(system, pen) {
  const np = system?.nopaint;
  let startPoint = null;
  if (np?.startDrag && isValidPoint(np.startDrag)) {
    startPoint = { x: np.startDrag.x, y: np.startDrag.y };
  } else if (pen && Number.isFinite(pen.x) && Number.isFinite(pen.y)) {
    startPoint = screenToPainting({ x: pen.x, y: pen.y }, system);
  }

  if (!isValidPoint(startPoint)) return;

  const key = `${startPoint.x},${startPoint.y}`;
  if (activeStroke.length === 0 || strokeStartKey !== key) {
    activeStroke.length = 0;
    strokeStartKey = null;
    pushUniquePaintingPoint(startPoint);
  if (lineDebugEnabled()) {
      console.log("🟡 LINE ensureStrokeInitialized", {
        source: np?.startDrag ? "startDrag" : "pen",
        startPoint,
        strokeStartKey,
        activeLength: activeStroke.length
      });
    }
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

function appendStrokePoint(system, pen) {
  const np = system?.nopaint;
  let point = null;
  if (np?.brush && isValidPoint(np.brush)) {
    point = { x: np.brush.x, y: np.brush.y };
  } else if (pen && Number.isFinite(pen.x) && Number.isFinite(pen.y)) {
    point = screenToPainting({ x: pen.x, y: pen.y }, system);
  }
  if (!point) return;
  pushUniquePaintingPoint(point);
  if (lineDebugEnabled()) {
    console.log("🔵 LINE appendStrokePoint", {
      point,
      source: np?.brush ? "nopaint.brush" : "pen",
      activeLength: activeStroke.length
    });
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

function ensureFinalPoint(system, pen) {
  const np = system?.nopaint;
  if (!np) return;
  if (np.finalEndPoint && isValidPoint(np.finalEndPoint)) {
    pushUniquePaintingPoint(np.finalEndPoint);
  } else if (pen && Number.isFinite(pen.x) && Number.isFinite(pen.y)) {
    const projected = screenToPainting({ x: pen.x, y: pen.y }, system);
    pushUniquePaintingPoint(projected);
  }
}

function overlay({ ink, color, mark, pen, system, net }) {
  if (!mark && !pen) return;
  const thickness = Math.max(1, Math.round(mark?.thickness ?? pen?.thickness ?? 1));
  const antialias = mark?.antialias ?? pen?.antialias ?? thickness <= 1;

  let points = [];
  const np = system?.nopaint;
  const penDrawing = !!(pen && pen.drawing);
  const paintingActive = penDrawing || (!!np?.startDrag && !!np?.brush);

  if (paintingActive && system?.nopaint) {
    ensureStrokeInitialized(system, pen);
    appendStrokePoint(system, pen);
    const paintingStroke = prepareStroke(activeStroke, thickness, antialias);
    points = paintingStrokeToScreen(paintingStroke, system);
  } else {
    points = collectLinePointsForOverlay({ mark, pen, system });
    if (points.length < 2 && activeStroke.length >= 2) {
      const paintingStroke = prepareStroke(activeStroke, thickness, antialias);
      points = paintingStrokeToScreen(paintingStroke, system);
    } else if (points.length >= 2) {
      points = prepareStroke(points, thickness, antialias);
    }
    if (!paintingActive && wasPenDrawing) {
      ensureFinalPoint(system, pen);
      if (points.length < 2 && activeStroke.length >= 2) {
        const paintingStroke = prepareStroke(activeStroke, thickness, antialias);
        points = paintingStrokeToScreen(paintingStroke, system);
      }
    }
  }

  if (points.length < 2) return;

  const lastPaintingPoint = activeStroke[activeStroke.length - 1] || null;
  const lastScreenPoint = points[points.length - 1] || null;
  const penScreenPoint = pen ? { x: pen.x, y: pen.y } : null;
  const screenDiff = penScreenPoint && lastScreenPoint
    ? {
        dx: Number.isFinite(lastScreenPoint.x - penScreenPoint.x)
          ? lastScreenPoint.x - penScreenPoint.x
          : null,
        dy: Number.isFinite(lastScreenPoint.y - penScreenPoint.y)
          ? lastScreenPoint.y - penScreenPoint.y
          : null,
      }
    : null;

  if (lineDebugEnabled()) {
    console.log("🧭 LINE overlay state", {
      penDrawing,
      paintingActive,
      pen: pen ? { x: pen.x, y: pen.y, drawing: pen.drawing } : null,
      npBrush: np?.brush ? { x: np.brush.x, y: np.brush.y } : null,
      npStartDrag: np?.startDrag ? { x: np.startDrag.x, y: np.startDrag.y } : null,
      npTranslation: np?.translation,
      npZoom: np?.zoomLevel,
      strokeLength: activeStroke.length,
      previewPoints: points.slice(0, 4),
      screenFirst: points[0],
      screenLast: lastScreenPoint,
      lastPaintingPoint,
      penScreenPoint,
      screenDiff
    });
    if (screenDiff) {
      console.log(
        `🧮 LINE overlay Δscreen-pen dx=${screenDiff.dx} dy=${screenDiff.dy} | ` +
        `pen=(${penScreenPoint?.x},${penScreenPoint?.y}) screen=(${lastScreenPoint?.x},${lastScreenPoint?.y}) ` +
        `paint=(${lastPaintingPoint?.x},${lastPaintingPoint?.y}) zoom=${np?.zoomLevel} translation=(${np?.translation?.x},${np?.translation?.y})`
      );
    }
  }

  if (lineDebugEnabled() && net?.log) {
    net.log("info", "line.overlay", {
      penDrawing,
      paintingActive,
      lastPaintingPoint,
      lastScreenPoint,
      penScreenPoint,
      screenDiff,
      translation: np?.translation,
      zoom: np?.zoomLevel,
      strokeLength: activeStroke.length
    });
  }

  try {
    const drawer = resolveInk(ink, color);
    drawLine(drawer, points, thickness, antialias);
  } catch (error) {
    console.error("🖊️ LINE overlay error:", error);
  }

  wasPenDrawing = paintingActive;
}

function lift({ ink, color, mark, pen, system, net }) {
  if (!mark && !pen) return false;
  const thickness = Math.max(1, Math.round(mark?.thickness ?? pen?.thickness ?? 1));
  const antialias = mark?.antialias ?? pen?.antialias ?? thickness <= 1;
  if (activeStroke.length >= 1) {
    ensureFinalPoint(system, pen);
  }
  const preferredPoints = activeStroke.length >= 2 ? clonePoints(activeStroke) : [];
  const points = preferredPoints.length >= 2 ? preferredPoints : collectLinePointsForLift({ mark, pen, system });
  if (points.length < 2) {
    console.warn("🖊️ LINE lift: not enough points to draw", points);
    activeStroke.length = 0;
    strokeStartKey = null;
    wasPenDrawing = false;
    return false;
  }

  try {
    const drawer = resolveInk(ink, color);
    const strokePoints = prepareStroke(points, thickness, antialias);
    drawLine(drawer, strokePoints, thickness, antialias);
  if (lineDebugEnabled()) {
      console.log("✅ LINE lift drew stroke", {
        strokePoints,
        thickness,
        antialias
      });
    }
  if (lineDebugEnabled() && net?.log) {
      net.log("info", "line.lift", {
        strokeLength: points.length,
        thickness,
        antialias,
        firstPoint: points[0],
        lastPoint: points[points.length - 1]
      });
    }
    activeStroke.length = 0;
    strokeStartKey = null;
    wasPenDrawing = false;
    return true;
  } catch (error) {
    console.error("🖊️ LINE lift error:", error);
    activeStroke.length = 0;
    strokeStartKey = null;
    wasPenDrawing = false;
    return false;
  }
}

export const system = "nopaint";

function boot({ colon } = {}) {
  if (Array.isArray(colon)) {
    if (colon.includes("quiet") || colon.includes("nodebug")) {
      debugLineLogs = false;
    } else if (colon.includes("debug")) {
      debugLineLogs = true;
    }
  }
}

export { overlay, lift, boot };
