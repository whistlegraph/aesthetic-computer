// Shape, 25.09.28
// Simple filled shape brush compatible with robo gestures and nopaint overlay/lift flow.

import { setFadeAlpha } from "../lib/fade-state.mjs";

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

function overlay({ ink, color, mark }) {
  if (!mark) return;
  const points = sanitizeShapePoints(mark.points, mark.closed ?? true);
  if (points.length < 3) return;

  try {
    const drawer = resolveInk(ink, color);
    const rounded = roundShapePoints(points);
    if (mark.outline) {
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
}

function lift({ ink, color, mark }) {
  if (!mark) return false;
  const points = sanitizeShapePoints(mark.points, mark.closed ?? true);
  if (points.length < 3) {
    console.warn("🔷 SHAPE lift: not enough points to draw", points);
    return false;
  }

  try {
    const drawer = resolveInk(ink, color);
    const rounded = roundShapePoints(points);
    if (mark.outline) {
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
    return true;
  } catch (error) {
    console.error("🔷 SHAPE lift error:", error);
    return false;
  }
}

export { overlay, lift };
