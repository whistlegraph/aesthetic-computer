// Robot automation system for aesthetic.computer
// Automates brush drawing by loading and executing brushes with synthetic pen data

import { generateNopaintHUDLabel } from "../lib/color-highlighting.mjs";
import { parseColor } from "../lib/num.mjs";

// Global robot state
const robotState = {
  active: false,
  state: "idle", // "idle", "painting", "lift"
  brushName: null,
  brushType: "box",
  currentStrategy: null,
  currentPath: null,
  pathQueue: [],
  completedPaths: [],
  frameCounter: 0,
  speed: 1.0,
  loadedBrush: null,
  painting: null,
  lastCompletedPath: null,
  currentProgress: 0,
  previewMark: null,
  lastPreviewMark: null,
  previousDrawPoint: null,
  allowMultipleLift: false, // Flag to control duplicate lift calls
  grid: { cols: 3, rows: 3 },
  targetQueueSize: 1,
  autoContinue: true,
  roboParams: null
};

function getNopaintTransform(nopaint) {
  const translation = nopaint?.translation || { x: 0, y: 0 };
  const zoom = nopaint?.zoomLevel || 1;
  return {
    translation: {
      x: translation.x || 0,
      y: translation.y || 0
    },
    zoom
  };
}

function paintingPointToScreen(point, transform) {
  const zoom = transform?.zoom || 1;
  const translation = transform?.translation || { x: 0, y: 0 };
  return {
    x: Math.round(translation.x + point.x * zoom),
    y: Math.round(translation.y + point.y * zoom)
  };
}

function paintingBoxToScreen(box, transform) {
  const zoom = transform?.zoom || 1;
  const translation = transform?.translation || { x: 0, y: 0 };
  return {
    x: Math.round(translation.x + box.x * zoom),
    y: Math.round(translation.y + box.y * zoom),
    w: Math.max(1, Math.round(box.w * zoom)),
    h: Math.max(1, Math.round(box.h * zoom))
  };
}

function paintingPointsToScreen(points, transform) {
  if (!Array.isArray(points)) return [];
  return points.map((point) => paintingPointToScreen(point, transform));
}

function clamp(value, min, max) {
  return Math.max(min, Math.min(max, value));
}

function distanceBetweenPoints(a, b) {
  const pointA = Array.isArray(a) ? { x: a[0], y: a[1] } : a;
  const pointB = Array.isArray(b) ? { x: b[0], y: b[1] } : b;
  const dx = (pointB?.x ?? 0) - (pointA?.x ?? 0);
  const dy = (pointB?.y ?? 0) - (pointA?.y ?? 0);
  return Math.hypot(dx, dy);
}

function computePathMetrics(points, { closed = false } = {}) {
  const normalized = (points || []).map((point) =>
    Array.isArray(point) ? { x: point[0], y: point[1] } : { x: point.x, y: point.y }
  );
  const distances = [];
  let totalLength = 0;

  for (let i = 0; i < normalized.length - 1; i++) {
    const distance = distanceBetweenPoints(normalized[i], normalized[i + 1]);
    distances.push(distance);
    totalLength += distance;
  }

  if (closed && normalized.length > 1) {
    const closingDistance = distanceBetweenPoints(
      normalized[normalized.length - 1],
      normalized[0]
    );
    distances.push(closingDistance);
    totalLength += closingDistance;
  }

  return { points: normalized, distances, totalLength, closed };
}

function buildProgressivePoints(metrics, progress) {
  if (!metrics?.points?.length) return [];
  const { points, distances, totalLength, closed } = metrics;
  if (totalLength <= 0) {
    return [...points];
  }

  const targetDistance = clamp(progress, 0, 1) * totalLength;
  const progressive = [points[0]];
  let accumulated = 0;

  const segmentCount = distances.length;
  for (let i = 0; i < segmentCount; i++) {
    const start = points[i];
    const end =
      i + 1 < points.length
        ? points[i + 1]
        : closed
        ? points[0]
        : points[points.length - 1];
    const segmentLength = distances[i] ?? 0;

    if (segmentLength === 0) {
      continue;
    }

    if (accumulated + segmentLength <= targetDistance) {
      progressive.push(end);
      accumulated += segmentLength;
      continue;
    }

    const remaining = targetDistance - accumulated;
    const ratio = clamp(segmentLength === 0 ? 0 : remaining / segmentLength, 0, 1);
    progressive.push({
      x: start.x + (end.x - start.x) * ratio,
      y: start.y + (end.y - start.y) * ratio
    });
    return progressive;
  }

  if (closed && progressive[progressive.length - 1] !== points[0]) {
    progressive.push(points[0]);
  }

  return progressive;
}

function convertPreviewMarkToScreen(preview, transform) {
  if (!preview) return null;
  switch (preview.type) {
    case "box":
      return paintingBoxToScreen(preview.painting, transform);
    case "line":
      return {
        points: paintingPointsToScreen(preview.painting.points, transform),
        thickness: preview.painting.thickness,
        antialias: preview.painting.antialias
      };
    case "shape":
      return {
        points: paintingPointsToScreen(preview.painting.points, transform).map(({ x, y }) => [x, y]),
        closed: preview.painting.closed,
        outline: preview.painting.outline
      };
    default:
      return preview.painting;
  }
}

const BRUSH_STRATEGIES = {
  box: {
    type: "box",
    getTouchPoint(path) {
      return path?.startPoint ?? null;
    },
    getDrawPoint(path, progress) {
      if (!path?.startPoint || !path?.endPoint) return null;
      const clamped = clamp(progress, 0, 1);
      return {
        x: path.startPoint.x + (path.endPoint.x - path.startPoint.x) * clamped,
        y: path.startPoint.y + (path.endPoint.y - path.startPoint.y) * clamped
      };
    },
    getPreviewMark(path, progress) {
      if (!path?.paintingMark) return null;
      const clamped = clamp(progress, 0, 1);
      const easing = 0.12 + clamped * 0.88;
      return {
        type: "box",
        painting: {
          x: path.paintingMark.x,
          y: path.paintingMark.y,
          w: Math.max(1, Math.round(path.paintingMark.w * easing)),
          h: Math.max(1, Math.round(path.paintingMark.h * easing))
        }
      };
    },
    getFinalMark(path) {
      if (!path?.paintingMark) return null;
      return {
        type: "box",
        painting: { ...path.paintingMark }
      };
    }
  },
  line: {
    type: "line",
    getTouchPoint(path) {
      return path?.startPoint ?? null;
    },
    getDrawPoint(path, progress) {
      if (!path?.metrics) return path?.endPoint ?? null;
      const progressivePoints = buildProgressivePoints(path.metrics, clamp(progress, 0, 1));
      return progressivePoints[progressivePoints.length - 1] ?? path.endPoint ?? null;
    },
    getPreviewMark(path, progress) {
      if (!path?.metrics) return null;
      const points = buildProgressivePoints(path.metrics, clamp(progress, 0, 1));
      if (points.length < 2 && path?.endPoint) {
        points.push(path.endPoint);
      }
      return {
        type: "line",
        painting: {
          points: points.map(({ x, y }) => ({ x, y })),
          thickness: path.paintingMark?.thickness ?? 1,
          antialias: path.paintingMark?.antialias ?? true
        }
      };
    },
    getFinalMark(path) {
      if (!path?.paintingMark) return null;
      return {
        type: "line",
        painting: {
          points: (path.paintingMark.points || []).map(({ x, y }) => ({ x, y })),
          thickness: path.paintingMark.thickness,
          antialias: path.paintingMark.antialias
        }
      };
    }
  },
  shape: {
    type: "shape",
    getTouchPoint(path) {
      return path?.startPoint ?? null;
    },
    getDrawPoint(path, progress) {
      if (!path?.metrics) return path?.endPoint ?? null;
      const progressivePoints = buildProgressivePoints(path.metrics, clamp(progress, 0, 1));
      return progressivePoints[progressivePoints.length - 1] ?? path.endPoint ?? null;
    },
    getPreviewMark(path, progress) {
      if (!path?.metrics) return null;
      const clamped = clamp(progress, 0, 1);
      let points = buildProgressivePoints(path.metrics, clamped);
      if (clamped >= 0.95) {
        points = [...path.metrics.points];
      }
      if (points.length < 3 && path.metrics.points.length >= 3) {
        points = path.metrics.points.slice(0, 3);
      }
      return {
        type: "shape",
        painting: {
          points: points.map(({ x, y }) => [x, y]),
          closed: true,
          outline: clamped < 1 ? true : path.paintingMark?.outline ?? false
        }
      };
    },
    getFinalMark(path) {
      if (!path?.paintingMark) return null;
      return {
        type: "shape",
        painting: {
          points: (path.paintingMark.points || []).map((pair) =>
            Array.isArray(pair) ? [pair[0], pair[1]] : [pair.x, pair.y]
          ),
          closed: path.paintingMark.closed ?? true,
          outline: path.paintingMark.outline ?? false
        }
      };
    }
  }
};

function getBrushStrategy(brushType) {
  return BRUSH_STRATEGIES[brushType] || BRUSH_STRATEGIES.box;
}

function inferBrushType(name) {
  if (!name) return "box";
  const normalized = `${name}`.toLowerCase();
  if (normalized.includes("line")) return "line";
  if (normalized.includes("shape")) return "shape";
  return "box";
}

function screenBoxToPainting(box, transform) {
  const zoom = transform?.zoom || 1;
  const translation = transform?.translation || { x: 0, y: 0 };
  return {
    x: Math.round((box.x - translation.x) / zoom),
    y: Math.round((box.y - translation.y) / zoom),
    w: Math.max(1, Math.round(box.w / zoom)),
    h: Math.max(1, Math.round(box.h / zoom))
  };
}

function getPaintingDimensions($api) {
  const painting = $api.system?.painting || robotState.painting;
  const width = painting?.width || $api.screen?.width || 0;
  const height = painting?.height || $api.screen?.height || 0;
  return { width, height };
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

function colorSpecToTokens(colorSpec) {
  const normalized = normalizeColorSpec(colorSpec);
  if (normalized === undefined || normalized === null) {
    return [];
  }

  if (typeof normalized === "string") {
    return [normalized];
  }

  if (Array.isArray(normalized)) {
    if (normalized.length > 0 && typeof normalized[0] === "string" && normalized[0].startsWith("fade")) {
      const [fade, ...rest] = normalized;
      return [fade, ...rest.map((value) => `${value}`)];
    }
    return normalized.map((value) => `${value}`);
  }

  if (typeof normalized === "object") {
    const { r, g, b, a } = normalized;
    const tokens = [];
    if (r !== undefined) tokens.push(`${Math.round(r)}`);
    if (g !== undefined) tokens.push(`${Math.round(g)}`);
    if (b !== undefined) tokens.push(`${Math.round(b)}`);
    if (a !== undefined) tokens.push(`${Math.round(a)}`);
    return tokens;
  }

  return [];
}

const MAX_HUD_TOKENS = 4;

function buildHudLabel({ colorSpec, brushType = "box", brushLabel }) {
  const colorTokens = colorSpecToTokens(colorSpec);
  const rawParams = colorTokens.length ? colorTokens : ["white"];
  const parsedColor = parseColor([...rawParams]);

  const displayTokens = rawParams.slice(0, MAX_HUD_TOKENS);
  const truncated = rawParams.length > MAX_HUD_TOKENS;

  const labelName = brushLabel || brushType || "box";
  let brushLabelText = generateNopaintHUDLabel(labelName, parsedColor, displayTokens);

  if (truncated) {
    brushLabelText += " \\gray\\…";
  }

  return `\\silver\\robo ${brushLabelText}`;
}

function scheduleHUDUpdate($api, path, options = {}) {
  if (!$api?.hud?.label || !path) return;
  const label = buildHudLabel({
    colorSpec: path.color,
    brushType: path.type || robotState.brushType,
    brushLabel: robotState.brushName
  });
  $api.hud.label(label);
}

function generatePathsForState($api, count) {
  const { width, height } = getPaintingDimensions($api);
  if (width <= 0 || height <= 0) {
    console.warn("🤖 Unable to generate paths - invalid painting dimensions", { width, height });
    return [];
  }

  const generator = new RoboPathGenerator();
  const brushType = robotState.brushType || robotState.brushName || "box";
  return generator.generate(
    width,
    height,
    robotState.grid?.cols || 3,
    robotState.grid?.rows || 3,
    count,
    brushType,
    robotState.roboParams || {}
  );
}

function replenishPathQueue($api) {
  if (!robotState.autoContinue) return;

  const desired = robotState.targetQueueSize || 1;
  while (robotState.pathQueue.length < desired) {
    const needed = desired - robotState.pathQueue.length;
    const newPaths = generatePathsForState($api, Math.max(1, needed));
    if (!newPaths.length) {
      console.warn("🤖 Replenish: no new paths generated, stopping auto-continue");
      robotState.autoContinue = false;
      break;
    }
    robotState.pathQueue.push(...newPaths);
    if (robotState.loadedBrush?.overlay && robotState.pathQueue.length) {
      const total = robotState.completedPaths.length + robotState.pathQueue.length;
      scheduleHUDUpdate($api, robotState.pathQueue[robotState.pathQueue.length - 1], {
        gridIndex: total - 1,
        total
      });
    }
  }

  if (robotState.pathQueue.length > 0) {
    robotState.active = true;
  }
}

// Robot path generator - creates drawing paths
class RoboPathGenerator {
  constructor() {
    this.currentColorIndex = 0;
    this.colors = [];
  }

  randomInt(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
  }

  randomCssColor() {
    const palette = [
      "red",
      "orange",
      "gold",
      "palegoldenrod",
      "khaki",
      "lawngreen",
      "springgreen",
      "aqua",
      "deepskyblue",
      "dodgerblue",
      "mediumslateblue",
      "mediumpurple",
      "violet",
      "pink",
      "hotpink",
      "deeppink",
      "crimson",
      "salmon",
      "tomato",
      "orangered",
      "yellow",
      "lightyellow",
      "mintcream",
      "azure",
      "lavender",
      "plum",
      "turquoise",
      "teal",
      "seagreen",
      "chartreuse"
    ];
    return palette[this.randomInt(0, palette.length - 1)];
  }

  randomHexColor() {
    const toHex = (value) => value.toString(16).padStart(2, "0");
    const r = this.randomInt(0, 255);
    const g = this.randomInt(0, 255);
    const b = this.randomInt(0, 255);
    return `#${toHex(r)}${toHex(g)}${toHex(b)}`;
  }

  randomColorArray(includeAlpha = true) {
    const r = this.randomInt(0, 255);
    const g = this.randomInt(0, 255);
    const b = this.randomInt(0, 255);
    if (!includeAlpha) {
      return [r, g, b];
    }
    const a = this.randomInt(160, 255);
    return [r, g, b, a];
  }

  randomFadeSpec() {
    const palette = [
      "red",
      "orange",
      "yellow",
      "green",
      "turquoise",
      "aqua",
      "blue",
      "indigo",
      "violet",
      "pink",
      "hotpink",
      "gold",
      "white",
      "black",
      "rainbow"
    ];
    const colorA = palette[this.randomInt(0, palette.length - 1)];
    let colorB = palette[this.randomInt(0, palette.length - 1)];
    if (colorA === colorB) {
      colorB = palette[(palette.indexOf(colorA) + 3) % palette.length];
    }
    const directions = ["horizontal", "vertical", "diagonal"];
    const direction = directions[this.randomInt(0, directions.length - 1)];
    const neat = Math.random() < 0.25;
    const fadePrefix = neat ? "fade:neat" : "fade";
    const fadeString = `${fadePrefix}:${colorA}-${colorB}:${direction}`;
    return [fadeString, 255];
  }

  generateColorSpec() {
    const roll = Math.random();
    if (roll < 0.35) {
      return this.randomCssColor();
    }
    if (roll < 0.6) {
      return this.randomHexColor();
    }
    if (roll < 0.85) {
      return this.randomColorArray(Math.random() < 0.5);
    }
    return this.randomFadeSpec();
  }

  // Generate rainbow colors using HSL
  generateRainbowColors(count) {
    const colors = [];
    for (let i = 0; i < count; i++) {
      const hue = (i / count) * 360;
      const hsl = `hsl(${hue}, 70%, 50%)`;
      // Convert HSL to RGB for nopaint system
      const rgb = this.hslToRgb(hue / 360, 0.7, 0.5);
      colors.push({
        r: Math.round(rgb[0] * 255),
        g: Math.round(rgb[1] * 255), 
        b: Math.round(rgb[2] * 255)
      });
    }
    return colors;
  }

  // HSL to RGB conversion
  hslToRgb(h, s, l) {
    let r, g, b;
    if (s === 0) {
      r = g = b = l;
    } else {
      const hue2rgb = (p, q, t) => {
        if (t < 0) t += 1;
        if (t > 1) t -= 1;
        if (t < 1/6) return p + (q - p) * 6 * t;
        if (t < 1/2) return q;
        if (t < 2/3) return p + (q - p) * (2/3 - t) * 6;
        return p;
      };
      const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
      const p = 2 * l - q;
      r = hue2rgb(p, q, h + 1/3);
      g = hue2rgb(p, q, h);
      b = hue2rgb(p, q, h - 1/3);
    }
    return [r, g, b];
  }

  // Get next color in sequence
  getNextColor() {
    if (this.colors.length === 0) return { r: 255, g: 255, b: 255 };
    const color = this.colors[this.currentColorIndex];
    this.currentColorIndex = (this.currentColorIndex + 1) % this.colors.length;
    return color;
  }

  // Generate grid pattern paths
  generate(width, height, gridCols, gridRows, count, brushType = "box", params = {}) {
    const spacing = params.spacing ?? 40;
    const positions = this.computeGridPositions(width, height, gridCols, gridRows, count, spacing);
    if (!positions.length) {
      console.warn("🤖 Robo generator: no positions available", {
        brushType,
        width,
        height,
        gridCols,
        gridRows
      });
      return [];
    }

    switch (brushType) {
      case "line":
        return this.generateLinePaths(positions, width, height, count);
      case "shape":
        return this.generateShapePaths(positions, width, height, count);
      default:
        return this.generateBoxPaths(positions, width, height, count);
    }
  }

  // Back-compat helper when only boxes are needed
  generateGrid(width, height, gridCols, gridRows, count, spacing = 40) {
    return this.generate(width, height, gridCols, gridRows, count, "box", { spacing });
  }

  computeGridPositions(width, height, gridCols, gridRows, count, spacing) {
    console.log(`🤖 Generating grid: ${gridCols}x${gridRows} on ${width}x${height} screen`);

    const adaptiveSpacing = Math.min(spacing, Math.min(width, height) / 10);
    console.log(`🤖 Adaptive spacing: ${adaptiveSpacing} (original: ${spacing})`);

    let finalCols = Math.max(1, gridCols);
    let finalRows = Math.max(1, gridRows);
    let availableWidth = width - adaptiveSpacing * (finalCols + 1);
    let availableHeight = height - adaptiveSpacing * (finalRows + 1);
    let cellWidth = Math.max(10, Math.floor(availableWidth / finalCols));
    let cellHeight = Math.max(10, Math.floor(availableHeight / finalRows));

    console.log(`🤖 Initial available space: ${availableWidth}x${availableHeight}`);
    console.log(`🤖 Initial cell size: ${cellWidth}x${cellHeight}, spacing: ${adaptiveSpacing}`);

    if (cellWidth < 10 || cellHeight < 10) {
      console.log("🤖 Cells too small, falling back to 2x2 grid");
      finalCols = Math.min(2, Math.max(1, gridCols));
      finalRows = Math.min(2, Math.max(1, gridRows));
      availableWidth = width - adaptiveSpacing * (finalCols + 1);
      availableHeight = height - adaptiveSpacing * (finalRows + 1);
      cellWidth = Math.max(10, Math.floor(availableWidth / finalCols));
      cellHeight = Math.max(10, Math.floor(availableHeight / finalRows));
      console.log(`🤖 Fallback available space: ${availableWidth}x${availableHeight}`);
      console.log(`🤖 Fallback cell size: ${cellWidth}x${cellHeight}`);
    }

    const totalWidth = finalCols * cellWidth + (finalCols - 1) * adaptiveSpacing;
    const totalHeight = finalRows * cellHeight + (finalRows - 1) * adaptiveSpacing;
    const startX = Math.max(adaptiveSpacing, (width - totalWidth) / 2);
    const startY = Math.max(adaptiveSpacing, (height - totalHeight) / 2);

    console.log(
      `🤖 Final grid positioned at (${startX}, ${startY}) with total size ${totalWidth}x${totalHeight}`
    );

    const positions = [];
    for (let row = 0; row < finalRows; row++) {
      for (let col = 0; col < finalCols; col++) {
        const x = startX + col * (cellWidth + adaptiveSpacing);
        const y = startY + row * (cellHeight + adaptiveSpacing);

        if (x < 0 || y < 0 || x + cellWidth > width || y + cellHeight > height) {
          console.log(
            `🤖 Rejected position: (${x}, ${y}) size ${cellWidth}x${cellHeight} - out of bounds`
          );
          continue;
        }

        const rounded = {
          x: clamp(Math.round(x), 0, Math.max(width - 1, 0)),
          y: clamp(Math.round(y), 0, Math.max(height - 1, 0)),
          w: Math.max(1, Math.round(cellWidth)),
          h: Math.max(1, Math.round(cellHeight))
        };

        if (rounded.x + rounded.w <= width && rounded.y + rounded.h <= height) {
          positions.push(rounded);
          console.log(
            `🤖 Valid position: (${rounded.x}, ${rounded.y}) size ${rounded.w}x${rounded.h}`
          );
        }
      }
    }

    console.log(`🤖 Generated ${positions.length} valid positions within screen bounds`);
    this.shuffle(positions);

    const desired = Math.max(count, 1) * 2;
    return positions.slice(0, Math.min(desired, positions.length));
  }

  generateBoxPaths(positions, width, height, count) {
    console.log("🤖 Preparing robo box paths", { count, positions: positions.length });
    const paths = [];
    const limit = Math.min(Math.max(count, 1), positions.length);

    for (let i = 0; i < limit; i++) {
      const pos = positions[i];
      const color = this.generateColorSpec();
      const widthScale = 0.4 + Math.random() * 0.6;
      const heightScale = 0.4 + Math.random() * 0.6;
      const variedWidth = Math.max(1, Math.round(pos.w * widthScale));
      const variedHeight = Math.max(1, Math.round(pos.h * heightScale));
      const maxOffsetX = Math.max(0, pos.w - variedWidth);
      const maxOffsetY = Math.max(0, pos.h - variedHeight);
      const offsetX = this.randomInt(0, maxOffsetX);
      const offsetY = this.randomInt(0, maxOffsetY);

      const finalBox = {
        x: clamp(pos.x + offsetX, 0, Math.max(width - variedWidth, 0)),
        y: clamp(pos.y + offsetY, 0, Math.max(height - variedHeight, 0)),
        w: variedWidth,
        h: variedHeight
      };

      const duration = Math.max(45, Math.round(Math.sqrt(finalBox.w * finalBox.h) * 3));
      console.log(
        `🤖 Box ${i + 1}: position (${finalBox.x}, ${finalBox.y}) size ${finalBox.w}x${finalBox.h}`,
        color
      );

      paths.push({
        type: "box",
        color,
        duration,
        paintingMark: finalBox,
        startPoint: { x: finalBox.x, y: finalBox.y },
        endPoint: {
          x: finalBox.x + Math.max(0, finalBox.w - 1),
          y: finalBox.y + Math.max(0, finalBox.h - 1)
        }
      });
    }

    console.log(`🤖 Generated ${paths.length} total box paths`);
    return paths;
  }

  generateLinePaths(positions, width, height, count) {
    console.log("🤖 Preparing robo line paths", { count, positions: positions.length });
    const paths = [];
    const limit = Math.min(Math.max(count, 1), positions.length);

    for (let i = 0; i < limit; i++) {
      const pos = positions[i];
      const color = this.generateColorSpec();
      const points = this.createLinePoints(pos, width, height);
      const metrics = computePathMetrics(points, { closed: false });
      const baseSize = Math.max(2, Math.min(pos.w, pos.h));
      const maxThickness = Math.max(1, Math.round(baseSize / 4));
      const thickness = this.randomInt(1, Math.max(1, maxThickness));
      const duration = Math.max(30, Math.round(Math.max(metrics.totalLength, baseSize) * 1.2));

      paths.push({
        type: "line",
        color,
        duration,
        paintingMark: {
          points: points.map(({ x, y }) => ({ x, y })),
          thickness,
          antialias: thickness <= 2
        },
        metrics,
        startPoint: points[0],
        endPoint: points[points.length - 1]
      });
    }

    console.log(`🤖 Generated ${paths.length} total line paths`);
    return paths;
  }

  generateShapePaths(positions, width, height, count) {
    console.log("🤖 Preparing robo shape paths", { count, positions: positions.length });
    const paths = [];
    const limit = Math.min(Math.max(count, 1), positions.length);

    for (let i = 0; i < limit; i++) {
      const pos = positions[i];
      const color = this.generateColorSpec();
      const outline = Math.random() < 0.4;
      const points = this.createShapePoints(pos, width, height);
      const metrics = computePathMetrics(points, { closed: true });
      const duration = Math.max(50, Math.round(Math.max(metrics.totalLength, pos.w + pos.h) * 0.9));

      paths.push({
        type: "shape",
        color,
        duration,
        paintingMark: {
          points: points.map(({ x, y }) => [x, y]),
          closed: true,
          outline
        },
        metrics,
        startPoint: points[0],
        endPoint: points[points.length - 1]
      });
    }

    console.log(`🤖 Generated ${paths.length} total shape paths`);
    return paths;
  }

  createLinePoints(base, width, height) {
    const pointCount = Math.random() < 0.4 ? 3 : 2;
    const points = [];
    const limitX = Math.max(width - 1, 0);
    const limitY = Math.max(height - 1, 0);

    const randomPoint = () => ({
      x: clamp(Math.round(base.x + Math.random() * base.w), 0, limitX),
      y: clamp(Math.round(base.y + Math.random() * base.h), 0, limitY)
    });

    for (let i = 0; i < pointCount; i++) {
      points.push(randomPoint());
    }

    // Ensure distinct endpoints to avoid zero-length strokes
    if (distanceBetweenPoints(points[0], points[points.length - 1]) < 2) {
      points[points.length - 1] = {
        x: clamp(
          points[0].x + this.randomInt(-Math.round(base.w / 2), Math.round(base.w / 2)),
          0,
          limitX
        ),
        y: clamp(
          points[0].y + this.randomInt(-Math.round(base.h / 2), Math.round(base.h / 2)),
          0,
          limitY
        )
      };
    }

    return points;
  }

  createShapePoints(base, width, height) {
    const vertexCount = this.randomInt(3, 6);
    const centerX = base.x + base.w / 2;
    const centerY = base.y + base.h / 2;
    const radiusX = Math.max(6, base.w / 2);
    const radiusY = Math.max(6, base.h / 2);
    const limitX = Math.max(width - 1, 0);
    const limitY = Math.max(height - 1, 0);

    const angles = [];
    for (let i = 0; i < vertexCount; i++) {
      angles.push(Math.random() * Math.PI * 2);
    }
    angles.sort((a, b) => a - b);

    const points = angles.map((angle) => {
      const stretchX = 0.45 + Math.random() * 0.55;
      const stretchY = 0.45 + Math.random() * 0.55;
      return {
        x: clamp(Math.round(centerX + Math.cos(angle) * radiusX * stretchX), 0, limitX),
        y: clamp(Math.round(centerY + Math.sin(angle) * radiusY * stretchY), 0, limitY)
      };
    });

    const uniquePoints = [];
    const seen = new Set();
    for (const point of points) {
      const key = `${point.x},${point.y}`;
      if (!seen.has(key)) {
        seen.add(key);
        uniquePoints.push(point);
      }
    }

    if (uniquePoints.length < 3) {
      const backupSize = Math.max(6, Math.min(base.w, base.h));
      uniquePoints.push(
        {
          x: clamp(Math.round(centerX - backupSize / 2), 0, limitX),
          y: clamp(Math.round(centerY - backupSize / 2), 0, limitY)
        },
        {
          x: clamp(Math.round(centerX + backupSize / 2), 0, limitX),
          y: clamp(Math.round(centerY - backupSize / 2), 0, limitY)
        },
        {
          x: clamp(Math.round(centerX), 0, limitX),
          y: clamp(Math.round(centerY + backupSize / 2), 0, limitY)
        }
      );
    }

    return uniquePoints.slice(0, 6);
  }

  shuffle(array) {
    for (let i = array.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [array[i], array[j]] = [array[j], array[i]];
    }
    return array;
  }
}

// Parse robot command parameters
function parseRoboParams(params) {
  const defaults = {
    speed: 1.0,
    grid: "3x3", 
    pattern: "random",
    count: 5
  };
  
  const parsed = { ...defaults };
  
  for (const param of params) {
    if (param.includes(":")) {
      const [key, value] = param.split(":");
      switch (key.toLowerCase()) {
        case "speed":
          parsed.speed = parseFloat(value) || defaults.speed;
          break;
        case "grid":
          parsed.grid = value || defaults.grid;
          break;
        case "pattern":
          parsed.pattern = value || defaults.pattern;
          break;
        case "count":
          parsed.count = parseInt(value) || defaults.count;
          break;
      }
    }
  }
  
  return parsed;
}

// Convert grid string to dimensions
function parseGrid(gridStr) {
  const parts = gridStr.split('x');
  if (parts.length !== 2) return { cols: 3, rows: 3 };
  
  const cols = parseInt(parts[0]) || 3;
  const rows = parseInt(parts[1]) || 3;
  return { cols, rows };
}

// Boot function - Initialize robot with brush and parameters  
async function boot($api) {
  const { params, screen, net } = $api;
  
  if (params.length === 0) {
    console.log("🤖 Usage: robo <brush> [options]");
    console.log("🤖 Options: speed:1.5, grid:3x3, pattern:random, count:5");
    return;
  }

  const brushName = params[0];
  robotState.brushType = inferBrushType(brushName);
  robotState.currentStrategy = getBrushStrategy(robotState.brushType);
  const roboParams = parseRoboParams(params.slice(1));

  console.log(`🤖 Starting robot for brush: ${brushName}`);
  console.log(`🤖 Settings:`, roboParams);

  // Load the target brush internally for robot operations
  try {
    // Fix localhost URLs if needed (for CSP compliance)
    let url = net.pieces;
    if (url.includes('https://localhost')) {
      url = url.replace('https://localhost', 'https://aesthetic.computer');
    }
    // Add cache-busting parameter to force reload
    const cacheBuster = Date.now();
    const fullUrl = `${url}/${brushName}.mjs?v=${cacheBuster}`;
    console.log(`🤖 Loading brush from URL: ${fullUrl}`);
    const brush = await import(fullUrl);
    
    // Create a painting buffer for the robot (like test.mjs does)
    const painting = $api.painting(screen.width, screen.height, (p) => {
      p.wipe(255, 255, 255, 0); // Transparent background
    });
    
  // Set up system for the brush (preserve existing system data like nopaint)
  $api.system = { ...($api.system || {}), painting };
    
    // Initialize the brush with the API (copy params so brush gets clean params)
    const brushApi = { ...$api, params: [...params.slice(1)] };
    brush?.boot?.(brushApi);
    
    console.log(`🤖 Screen dimensions: ${screen.width}x${screen.height}`);

    // Parse grid dimensions
    const { cols, rows } = parseGrid(roboParams.grid);
    console.log(`🤖 Grid setup: ${cols}x${rows}`);

  robotState.grid = { cols, rows };
  const desiredCount = Math.max(1, roboParams.count || 1);
  robotState.targetQueueSize = desiredCount;
  const pattern = (roboParams.pattern || "random").toLowerCase();
  const autoPatterns = ["random", "loop", "continuous", "stream"];
  robotState.autoContinue = autoPatterns.includes(pattern);
  robotState.roboParams = roboParams;

    const paintingWidth = $api.system?.painting?.width || screen.width;
    const paintingHeight = $api.system?.painting?.height || screen.height;
    const transform = getNopaintTransform($api.system?.nopaint);

    console.log("🤖 Painting context:", {
      paintingWidth,
      paintingHeight,
      translation: transform.translation,
      zoom: transform.zoom
    });

    // Generate paths in painting coordinates using the robot path generator
    const generator = new RoboPathGenerator();
    const paths = generator.generateGrid(
      paintingWidth,
      paintingHeight,
      cols,
      rows,
      roboParams.count
    );

    if (paths.length === 0) {
      console.log("🤖 No paths generated");
      return;
    }

  console.log(`🤖 Generated ${paths.length} path(s) for execution in painting space`);

    scheduleHUDUpdate($api, paths[0], { gridIndex: 0, total: paths.length });

    // Initialize robot state (modify properties instead of reassigning const)
    robotState.active = true;
    robotState.state = "idle"; // Start in idle state
    robotState.brushName = brushName;
  robotState.brushType = inferBrushType(brushName);
  robotState.currentStrategy = getBrushStrategy(robotState.brushType);
    robotState.pathQueue = [...paths];
    robotState.currentPath = null;
    robotState.completedPaths = [];
    robotState.frameCounter = 0;
    robotState.speed = roboParams.speed;
    robotState.loadedBrush = brush;
    robotState.painting = painting;
  robotState.lastCompletedPath = null;
  robotState.currentProgress = 0;
  robotState.previewMark = null;
  robotState.lastPreviewMark = null;
  robotState.previousDrawPoint = null;
    robotState.allowMultipleLift = false;

    console.log("🤖 Robot initialized and ready for execution");
    
  } catch (error) {
    console.error(`🤖 Failed to load brush ${brushName}:`, error);
  }
}

// Paint function - execute the loaded brush with robot-generated pen data
function paint($api) {
  if (!robotState.active || !robotState.loadedBrush) return;
  
  // Don't override the existing system - just ensure painting is set
  if (!$api.system.painting) {
    $api.system.painting = robotState.painting;
  }
  
  // The robot doesn't directly call brush paint - it sends events through the robo API
  // which then get processed by the nopaint system
  console.log(`🤖 Paint called - robot state: ${robotState.state}`);
  
  // Monitor nopaint state processing
  if ($api.system?.nopaint) {
    const np = $api.system.nopaint;
    console.log("🤖 Paint: nopaint state check:", {
      needsPresent: np.needsPresent,
      needsBake: np.needsBake,
      bufferExists: !!np.buffer,
      robotActive: robotState.active,
      robotState: robotState.state
    });
    
    // If needsPresent is true, log that we're expecting a present call
    if (np.needsPresent) {
      console.log("🔍 NOPAINT PRESENT FLAG IS TRUE - expecting present() to be called soon");
    }
    
    // Return true to force continuous rendering when nopaint has pending updates
    if (np.needsPresent || np.needsBake) {
      return true;
    }
  }
  
  // Note: Display is handled by nopaint system, not by manual paste
  // The nopaint system will handle presenting the final result after baking
}

// Sim function - 120fps locked timing for robot execution
function sim($api) {
  if (!robotState.active || !robotState.loadedBrush) return;
  
  const { simCount, needsPaint, system } = $api;
  
  // Force continuous rendering if nopaint has pending display updates
  if (system?.nopaint && (system.nopaint.needsPresent || system.nopaint.needsBake)) {
    needsPaint();
  }
  
  // Speed control: advance robot logic every N sim frames
  const speedFrames = Math.max(1, Math.round(120 / (60 * robotState.speed)));
  
  if (simCount % BigInt(speedFrames) === 0n) {
    advanceRobotLogic($api);
    needsPaint(); // Request paint update when robot state changes
  }
}

// Advance robot logic - progress through paths
function advanceRobotLogic($api) {
  const transform = getNopaintTransform($api.system?.nopaint);

  if (!robotState.currentPath && robotState.pathQueue.length > 0) {
    const nextPath = robotState.pathQueue.shift();
    robotState.currentPath = nextPath;
    robotState.currentStrategy = getBrushStrategy(nextPath.type || robotState.brushType);
    robotState.frameCounter = 0;
    robotState.state = "painting";
    robotState.currentProgress = 0;
    robotState.previewMark = robotState.currentStrategy.getPreviewMark(nextPath, 0);
    robotState.lastPreviewMark = robotState.previewMark;
    robotState.previousDrawPoint = robotState.currentStrategy.getTouchPoint(nextPath) || nextPath.startPoint;

    const currentIndex = robotState.completedPaths.length;
    const totalPlanned = currentIndex + 1 + robotState.pathQueue.length;
    scheduleHUDUpdate($api, nextPath, { gridIndex: currentIndex, total: totalPlanned });

    const touchPoint = robotState.previousDrawPoint;
    if (touchPoint) {
      const startScreen = paintingPointToScreen(touchPoint, transform);
      console.log(
        `🤖 Starting path ${currentIndex + 1} (${nextPath.type}) at screen (${startScreen.x}, ${startScreen.y})`
      );
      $api.robo.touch(startScreen.x, startScreen.y);
    }
  }

  const path = robotState.currentPath;
  if (!path) {
    return;
  }

  const strategy = robotState.currentStrategy || getBrushStrategy(path.type || robotState.brushType);
  robotState.frameCounter += 1;
  const durationFrames = Math.max(1, path.duration);
  const progress = Math.min(1, robotState.frameCounter / durationFrames);
  robotState.currentProgress = progress;

  const previousPoint = robotState.previousDrawPoint || strategy.getTouchPoint(path) || path.startPoint;
  const drawPoint = strategy.getDrawPoint(path, progress) || path.endPoint || previousPoint;

  if (drawPoint && previousPoint) {
    const currentScreen = paintingPointToScreen(drawPoint, transform);
    const prevScreen = paintingPointToScreen(previousPoint, transform);
    $api.robo.draw(currentScreen.x, currentScreen.y, prevScreen.x, prevScreen.y);
    robotState.previousDrawPoint = drawPoint;
  }

  const previewMark = strategy.getPreviewMark(path, progress);
  if (previewMark) {
    robotState.previewMark = previewMark;
    robotState.lastPreviewMark = previewMark;
  }

  if (progress >= 1) {
    robotState.state = "lift";
    robotState.allowMultipleLift = true;

    const finalMark = strategy.getFinalMark(path);
    if (finalMark) {
      robotState.previewMark = finalMark;
      robotState.lastPreviewMark = finalMark;
      robotState.lastCompletedPath = {
        type: finalMark.type,
        paintingMark: finalMark.painting,
        color: path.color
      };
    } else {
      robotState.lastCompletedPath = {
        type: path.type || robotState.brushType,
        paintingMark: null,
        color: path.color
      };
    }

    const liftPoint = strategy.getDrawPoint(path, 1) || path.endPoint || robotState.previousDrawPoint;
    const liftScreen = liftPoint ? paintingPointToScreen(liftPoint, transform) : null;

    robotState.completedPaths.push(path);
    robotState.currentPath = null;
    robotState.frameCounter = 0;
    robotState.currentProgress = 0;
    robotState.previousDrawPoint = null;

    replenishPathQueue($api);

    if (liftScreen) {
      $api.robo.lift(liftScreen.x, liftScreen.y);
    } else {
      $api.robo.lift(0, 0);
    }

    if (robotState.pathQueue.length === 0 && !robotState.autoContinue) {
      robotState.active = false;
    }
  }
}

// Act function - handle user interactions (required by disk system)
function act($api) {
  const { event: e } = $api;
  
  // Monitor ALL events to debug synthetic robo events
  if (e) {
    console.log(`🔍 ACT EVENT: type="${e.type}" device="${e.device}" x=${e.x} y=${e.y} pressure=${e.pressure}`);
    
    // Special logging for robot events
    if (e.device === "robot") {
      console.log("🤖 ROBOT EVENT DETECTED:", {
        type: e.type,
        hasIs: typeof e.is === 'function',
        isLift: e.is ? e.is('lift:1') : 'no is function',
        nopaintState: $api.system?.nopaint ? {
          needsPresent: $api.system.nopaint.needsPresent,
          needsBake: $api.system.nopaint.needsBake
        } : 'no nopaint'
      });
    }
  }
  
  // Pass through to loaded brush if needed
  if (robotState.loadedBrush?.act) {
    try {
      robotState.loadedBrush.act($api);
    } catch (error) {
      console.error("🤖 Error calling brush act:", error);
    }
  }
}

// Brush delegation functions - called by nopaint system
function overlay(api) {
  if (robotState.state === "lift") {
    console.log("🤖 ROBO overlay: currently lifting, delegating to lift");
    return lift(api);
  }

  if (!robotState.loadedBrush?.overlay) {
    console.warn("🤖 ROBO overlay: loaded brush has no overlay function");
    return null;
  }

  const transform = getNopaintTransform(api.system?.nopaint);
  const strategy = robotState.currentStrategy || getBrushStrategy(robotState.brushType);

  let preview = robotState.previewMark || robotState.lastPreviewMark;
  if (!preview && robotState.currentPath) {
    preview = strategy.getPreviewMark(robotState.currentPath, robotState.currentProgress || 0);
    if (preview) {
      robotState.previewMark = preview;
      robotState.lastPreviewMark = preview;
    }
  }

  if (!preview) {
    console.warn("🤖 ROBO overlay: no preview mark available");
    return null;
  }

  const screenMark = convertPreviewMarkToScreen(preview, transform);
  if (!screenMark) {
    console.warn("🤖 ROBO overlay: failed to convert preview mark to screen coordinates", preview);
    return null;
  }

  const colorCandidate =
    robotState.currentPath?.color ??
    robotState.lastCompletedPath?.color ??
    api.color;
  const colorSpec = normalizeColorSpec(colorCandidate);

  const brushApi = {
    ...api,
    color: colorSpec,
    mark: screenMark,
    system: {
      ...(api.system || {}),
      robotActive: true
    }
  };

  try {
    return robotState.loadedBrush.overlay(brushApi);
  } catch (error) {
    console.error("🤖 ROBO overlay: error invoking brush overlay", error);
    return null;
  }
}

function lift(api) {
  console.log("🤖🤖🤖 ROBO LIFT FUNCTION CALLED! 🤖🤖🤖");
  console.log(`🤖 ROBO LIFT CALLED - current state: ${robotState.state} - this should trigger box drawing!`);
  
  // Prevent multiple lift calls when already in idle state (fix for duplicate calls)
  if (robotState.state === "idle" && !robotState.allowMultipleLift) {
    console.log("🤖 ROBO LIFT: Already in idle state, ignoring duplicate lift call");
    return;
  }
  
  console.log("🤖 Lift API context:", {
    hasInk: !!api.ink,
    inkType: typeof api.ink,
    hasColor: !!api.color,
    color: api.color,
    hasMark: !!api.mark,
    mark: api.mark,
    hasPen: !!api.pen,
    pen: api.pen,
    screenSize: { width: api.screen?.width, height: api.screen?.height },
    hasCurrentPath: !!robotState.currentPath,
    hasLoadedBrush: !!robotState.loadedBrush,
    hasLoadedBrushLift: !!robotState.loadedBrush?.lift
  });
  
  if (robotState.loadedBrush?.lift) {
    console.log("📦 ROBO: About to call box brush lift function");
    
    // Use lastCompletedPath data if available, otherwise use API data
    const lastCompleted = robotState.lastCompletedPath;
    let finalBox, finalColor;
    
    if (lastCompleted && lastCompleted.boxDimensions) {
      finalBox = lastCompleted.boxDimensions;
      finalColor = normalizeColorSpec(lastCompleted.color);
      console.log("📦 ROBO: Using last completed path data:", { box: finalBox, color: finalColor });
    } else {
      // Fallback to API data
      if (api.mark) {
        const screenMark = api.mark;
        finalBox = screenBoxToPainting(screenMark, getNopaintTransform(api.system?.nopaint));
      }
      finalColor = normalizeColorSpec(api.color) || [255, 255, 255];
      console.log("📦 ROBO: Using API data (no completed path):", { box: finalBox, color: finalColor });
    }
    
    if (!finalBox || finalBox.w <= 0 || finalBox.h <= 0) {
      console.warn("📦 ROBO: No valid final box to paint during lift", { finalBox });
      robotState.state = "idle";
      robotState.allowMultipleLift = false;
      return;
    }
    
    // Create proper context for the brush lift function
    const brushApi = {
      ...api,
      ink: api.ink,
      color: finalColor,
      mark: finalBox,
      system: api.system // Make sure to pass the system context to lift
    };
    
    console.log("📦 ROBO: Calling box brush lift with:", {
      hasInk: !!brushApi.ink,
      color: brushApi.color,
      mark: brushApi.mark,
      markIsValid: !!(brushApi.mark && brushApi.mark.x !== undefined && brushApi.mark.y !== undefined && brushApi.mark.w > 0 && brushApi.mark.h > 0)
    });
    
    console.log("📦 ROBO: About to call lift function:", {
      liftExists: !!robotState.loadedBrush.lift,
      liftType: typeof robotState.loadedBrush.lift,
      liftString: robotState.loadedBrush.lift?.toString()?.substring(0, 200)
    });
    
    try {
      // Call the box brush lift function - this should draw the final box
      const result = robotState.loadedBrush.lift(brushApi);
      console.log("📦 ROBO: Box brush lift completed, result:", result);

      // Now that lift is complete, transition state back to idle and check for next path
      robotState.state = "idle";
      robotState.allowMultipleLift = false; // Reset flag to prevent duplicate calls
      console.log("🤖 Lift completed - state set to idle");
      
      // If there are more paths in the queue, they will be started on the next sim cycle
      if (robotState.pathQueue.length > 0) {
        console.log(`🤖 Lift completed, ${robotState.pathQueue.length} paths remaining - will start next path`);
      } else {
        console.log("🤖 Lift completed, all paths finished");
      }
      
      return result;
    } catch (error) {
      console.error("📦 ROBO: Error calling box brush lift:", error);
      // Even on error, transition back to idle
      robotState.state = "idle";
      robotState.allowMultipleLift = false; // Reset flag on error too
      console.log("🤖 Lift error - state set to idle");
    }
  } else {
    console.warn("🤖 ROBO: No lift function in loaded brush!");
    // If no lift function, still transition back to idle
    robotState.state = "idle";
    robotState.allowMultipleLift = false; // Reset flag here too
    console.log("🤖 No lift function - state set to idle");
  }
}

// 🍪 Bake - Transfer drawn content to the final painting
function bake({ paste, system, page, needsPaint }) {
  console.log("🤖🍪 ROBO BAKE CALLED! 🍪🤖");
  console.log("🤖 Robo bake: Current nopaint state:", {
    hasBuffer: !!system.nopaint.buffer,
    needsPresent: system.nopaint.needsPresent,
    needsBake: system.nopaint.needsBake,
    bufferSize: system.nopaint.buffer ? `${system.nopaint.buffer.width}x${system.nopaint.buffer.height}` : "none"
  });
  
  if (system.nopaint.buffer) {
    console.log("🤖 BAKING: About to paste nopaint buffer to final painting");
    
    // Try direct paste to main painting buffer
    const result = paste(system.nopaint.buffer);
    console.log("🤖 BAKING: Paste result:", result);
    
    console.log("🤖 BAKING: Pasted buffer, now wiping nopaint buffer");
    page(system.nopaint.buffer).wipe(255, 255, 255, 0);
    console.log("🤖🍪 ROBO BAKE COMPLETED - Successfully transferred to final painting! 🍪🤖");
    
    // Reset nopaint flags to indicate baking is done
    system.nopaint.needsBake = false;
    system.nopaint.needsPresent = false;
    console.log("🤖 BAKING: Reset nopaint flags after baking");
    
    // Force immediate display update of the main painting
    needsPaint();
    console.log("🤖 BAKING: Called needsPaint() to display final result");
    
    // Signal that baking is complete and changes were made
    return true;
  } else {
    console.warn("🤖 Robo bake: No nopaint buffer to paste - nothing to bake");
    return false;
  }
}

export { boot, paint, sim, act, overlay, lift, bake };

// Export system type so disk.mjs recognizes this as a nopaint piece
export const system = "nopaint";

// Debug: Log that lift function is exported
console.log("🤖 ROBO MODULE: lift function exported:", typeof lift);