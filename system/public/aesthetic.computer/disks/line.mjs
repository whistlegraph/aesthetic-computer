// Line, 25.09.30
// Simple line brush

export const system = "nopaint";

function getLineEndpoints(mark) {
  if (!mark) return null;

  if (
    typeof mark.x === "number" &&
    typeof mark.y === "number" &&
    typeof mark.w === "number" &&
    typeof mark.h === "number"
  ) {
    return {
      x1: mark.x,
      y1: mark.y,
      x2: mark.x + mark.w,
      y2: mark.y + mark.h
    };
  }

  if (Array.isArray(mark) && mark.length >= 2) {
    const first = mark[0];
    const last = mark[mark.length - 1];
    const toPoint = (point) =>
      Array.isArray(point)
        ? { x: point[0], y: point[1] }
        : { x: point?.x, y: point?.y };
    const start = toPoint(first);
    const end = toPoint(last);
    if (Number.isFinite(start.x) && Number.isFinite(start.y) && Number.isFinite(end.x) && Number.isFinite(end.y)) {
      return { x1: start.x, y1: start.y, x2: end.x, y2: end.y };
    }
  }

  if (mark?.points && Array.isArray(mark.points) && mark.points.length >= 2) {
    const points = mark.points;
    return getLineEndpoints(points);
  }

  return null;
}

function formatAngle(angleDegrees) {
  if (!Number.isFinite(angleDegrees)) return null;
  const normalized = ((angleDegrees % 360) + 360) % 360;
  const fixed = normalized.toFixed(2);
  return fixed.replace(/\.00$/, "");
}

function ensureFadeDirectionString(fadeString, angleDegrees) {
  if (typeof fadeString !== "string" || !fadeString.startsWith("fade:")) {
    return fadeString;
  }

  const parts = fadeString.split(":");
  if (parts.length < 2) return fadeString;

  let hasNeat = false;
  let type = null;
  let directionExists = false;

  for (let i = 1; i < parts.length; i += 1) {
    const segment = parts[i];
    if (segment === "neat") {
      hasNeat = true;
      continue;
    }
    if (!type) {
      type = segment;
      continue;
    }
    directionExists = true;
    break;
  }

  if (!type || directionExists) {
    return fadeString;
  }

  const angle = formatAngle(angleDegrees);
  if (angle === null) return fadeString;

  return hasNeat ? `fade:neat:${type}:${angle}` : `fade:${type}:${angle}`;
}

function alignFadeColorToLine(color, endpoints) {
  if (!color || !endpoints) return color;

  const { x1, y1, x2, y2 } = endpoints;
  const angle = Math.atan2(y2 - y1, x2 - x1) * (180 / Math.PI);

  if (typeof color === "string") {
    return ensureFadeDirectionString(color, angle);
  }

  if (Array.isArray(color)) {
    if (color.length > 0 && typeof color[0] === "string") {
      const updated = ensureFadeDirectionString(color[0], angle);
      if (updated !== color[0]) {
        return [updated, ...color.slice(1)];
      }
    }
    return color;
  }

  if (typeof color === "object" && color.type === "fade") {
    const updated = ensureFadeDirectionString(color.fadeString, angle);
    if (updated !== color.fadeString) {
      return { ...color, fadeString: updated };
    }
  }

  return color;
}

function overlay({ ink, color, mark }) {
  const endpoints = getLineEndpoints(mark);
  if (!endpoints) return;

  const adjustedColor = alignFadeColorToLine(color, endpoints);

  try {
    ink(adjustedColor).line(endpoints.x1, endpoints.y1, endpoints.x2, endpoints.y2);
  } catch (error) {
    console.error("📏 LINE overlay error:", error, { color: adjustedColor, mark });
  }
}

function lift({ ink, color, mark }) {
  const endpoints = getLineEndpoints(mark);
  if (!endpoints) return false;

  const adjustedColor = alignFadeColorToLine(color, endpoints);

  try {
    ink(adjustedColor).line(endpoints.x1, endpoints.y1, endpoints.x2, endpoints.y2);
    return true;
  } catch (error) {
    console.error("📏 LINE lift error:", error, { color: adjustedColor, mark });
    return false;
  }
}

export { overlay, lift };
