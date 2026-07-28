// Deterministic pixel transforms recovered from the Construct-era No Paint export.
// Evidence: nopaint.art/data.json event sheets named by each `source.sheet` value.
// The export's plugin/action ids are retained in source notes where their implementation
// is not present in the event sheet; those contracts are honestly labelled reconstructed.

const clampByte = value => Math.max(0, Math.min(255, Math.round(value)));
const freeze = value => Object.freeze(value);
const pixelIndex = (x, y, width) => (y * width + x) * 4;
const copyPixel = (source, target, fromX, fromY, toX, toY, width) => {
  const from = pixelIndex(fromX, fromY, width), to = pixelIndex(toX, toY, width);
  for (let channel = 0; channel < 4; channel += 1) target[to + channel] = source[from + channel];
};
const mapPixels = (pixels, callback) => {
  const output = new Uint8ClampedArray(pixels);
  for (let index = 0; index < output.length; index += 4) callback(output, index);
  return output;
};
const wrapped = (value, size) => ((value % size) + size) % size;

function contract(slug, label, fidelity, evidence, defaults, applyPixels) {
  return freeze({
    version: 1, slug, label, compatible: true, kind: "pixel-transform", fidelity,
    source: freeze({ file: "/nopaint.art/data.json", sheet: label, ...evidence }),
    generate({ random, width, height, base }) {
      const parameters = freeze(defaults({ random, width, height }));
      return freeze({ ...base, kind: slug, transform: slug, brush: freeze({
        slug, params: freeze([]), colon: freeze([]), parameters,
      }) });
    },
    applyPixels,
  });
}

export const mirrorTransform = contract("mirror", "Mirror", "event-sheet-exact",
  { variables: freeze(["position", "direction"]), renderFunction: "MirrorRender", pixelAction: 180 },
  ({ random, width }) => ({ axis: Math.floor(random() * width), direction: random() < .5 ? -1 : 1 }),
  (pixels, width, height, { axis, direction }) => {
    const output = new Uint8ClampedArray(pixels);
    for (let y = 0; y < height; y += 1) for (let x = 0; x < width; x += 1) {
      const reflected = Math.max(0, Math.min(width - 1, 2 * axis - x));
      if ((direction < 0 && x > axis) || (direction >= 0 && x < axis)) copyPixel(pixels, output, reflected, y, x, y, width);
    }
    return output;
  });

export const flipTransform = contract("flip", "Flip", "event-sheet-exact",
  { variables: freeze(["vertically", "sound"]), pixelAction: 193, timerSeconds: 1 },
  ({ random }) => ({ vertically: random() < .5 }),
  (pixels, width, height, { vertically }) => {
    const output = new Uint8ClampedArray(pixels.length);
    for (let y = 0; y < height; y += 1) for (let x = 0; x < width; x += 1)
      copyPixel(pixels, output, x, y, vertically ? x : width - 1 - x, vertically ? height - 1 - y : y, width);
    return output;
  });

export const invertTransform = contract("invert", "Invert", "event-sheet-exact",
  { variables: freeze(["inverted"]), audio: freeze(["invert - on", "invert - off"]) },
  () => ({ inverted: true }),
  pixels => mapPixels(pixels, (output, index) => {
    output[index] = 255 - output[index]; output[index + 1] = 255 - output[index + 1]; output[index + 2] = 255 - output[index + 2];
  }));

export const saturateTransform = contract("saturate", "Saturate", "event-sheet-equivalent",
  { variables: freeze(["saturateShift", "direction"]), timerTagExpression: 397 },
  ({ random }) => ({ shift: .1 + random() * .4, direction: random() < .5 ? -1 : 1 }),
  (pixels, width, height, { shift, direction }) => mapPixels(pixels, (output, index) => {
    const r = output[index], g = output[index + 1], b = output[index + 2];
    const gray = .2126 * r + .7152 * g + .0722 * b, amount = direction * shift;
    output[index] = clampByte(gray + (r - gray) * (1 + amount));
    output[index + 1] = clampByte(gray + (g - gray) * (1 + amount));
    output[index + 2] = clampByte(gray + (b - gray) * (1 + amount));
  }));

export const contrastTransform = contract("contrast", "Contrast", "event-sheet-equivalent",
  { variables: freeze(["contrastShift", "direction"]), timerTagExpression: 238 },
  ({ random }) => ({ shift: .1 + random() * .4, direction: random() < .5 ? -1 : 1 }),
  (pixels, width, height, { shift, direction }) => mapPixels(pixels, (output, index) => {
    const factor = 1 + direction * shift;
    for (let channel = 0; channel < 3; channel += 1) output[index + channel] = clampByte((output[index + channel] - 128) * factor + 128);
  }));

export const scrollTransform = contract("scroll", "Scroll", "event-sheet-equivalent",
  { variables: freeze(["gap", "horizontal"]), renderFunction: "ScrollRender", pixelAction: 179 },
  ({ random, width, height }) => ({ horizontal: random() < .5, offset: 1 + Math.floor(random() * Math.max(width, height)) }),
  (pixels, width, height, { horizontal, offset }) => {
    const output = new Uint8ClampedArray(pixels.length);
    for (let y = 0; y < height; y += 1) for (let x = 0; x < width; x += 1)
      copyPixel(pixels, output, x, y, horizontal ? wrapped(x + offset, width) : x, horizontal ? y : wrapped(y + offset, height), width);
    return output;
  });

function centeredSample(pixels, width, height, scale, rotation = 0) {
  const output = new Uint8ClampedArray(pixels.length), cx = (width - 1) / 2, cy = (height - 1) / 2;
  const cosine = Math.cos(rotation), sine = Math.sin(rotation);
  for (let y = 0; y < height; y += 1) for (let x = 0; x < width; x += 1) {
    const dx = (x - cx) / scale, dy = (y - cy) / scale;
    const sx = Math.round(cx + dx * cosine + dy * sine), sy = Math.round(cy - dx * sine + dy * cosine);
    if (sx >= 0 && sx < width && sy >= 0 && sy < height) copyPixel(pixels, output, sx, sy, x, y, width);
  }
  return output;
}

export const zoomTransform = contract("zoom", "Zoom", "event-sheet-equivalent",
  { variables: freeze(["zoomMax", "zoomLevel", "zoomIn", "x", "y"]), audio: freeze(["zoom - in", "zoom - out"]) },
  ({ random }) => ({ zoomIn: random() < .5, level: 1 + Math.floor(random() * 4) }),
  (pixels, width, height, { zoomIn, level }) => centeredSample(pixels, width, height, zoomIn ? 1 + level / 8 : 1 / (1 + level / 8)));

export const recurseTransform = contract("recurse", "Recurse", "event-sheet-equivalent",
  { variables: freeze(["zoomLevel", "zoomDivision", "zoomOut", "x", "y"]), pixelAction: 258 },
  ({ random }) => ({ zoomOut: random() < .5, division: 2 + Math.floor(random() * 9) }),
  (pixels, width, height, { zoomOut, division }) => centeredSample(pixels, width, height, zoomOut ? 1 - 1 / division : 1 + 1 / division));

export const spinTransform = contract("spin", "Spin", "event-sheet-equivalent",
  { variables: freeze(["direction", "spinAngle"]), audio: freeze(["spin - theme"]) },
  ({ random }) => ({ direction: random() < .5 ? -1 : 1, angle: 5 + Math.floor(random() * 55) }),
  (pixels, width, height, { direction, angle }) => centeredSample(pixels, width, height, 1, direction * angle * Math.PI / 180));

export const turnTransform = contract("turn", "Turn", "event-sheet-equivalent",
  { variables: freeze(["rotation", "direction"]), notes: 4 },
  ({ random }) => ({ direction: random() < .5 ? -1 : 1, quarterTurns: 1 + Math.floor(random() * 3) }),
  (pixels, width, height, { direction, quarterTurns }) => centeredSample(pixels, width, height, 1, direction * quarterTurns * Math.PI / 2));

export const sharpenTransform = contract("sharpen", "Sharpen", "reconstructed-plugin-equivalent",
  { variables: freeze(["shift", "direction"]), pluginActions: freeze([152, 60]), limitation: "C3 plugin kernels are numeric-only in export; uses the standard 3x3 sharpen kernel." },
  ({ random }) => ({ amount: .25 + random() * .75 }),
  (pixels, width, height, { amount }) => {
    const output = new Uint8ClampedArray(pixels);
    for (let y = 1; y < height - 1; y += 1) for (let x = 1; x < width - 1; x += 1) for (let channel = 0; channel < 3; channel += 1) {
      const index = pixelIndex(x, y, width) + channel;
      const neighbours = pixels[index - 4] + pixels[index + 4] + pixels[index - width * 4] + pixels[index + width * 4];
      output[index] = clampByte(pixels[index] * (1 + 4 * amount) - neighbours * amount);
    }
    return output;
  });

export const quicksandTransform = contract("quicksand", "Quicksand", "reconstructed-plugin-equivalent",
  { variables: freeze(["swirlAmount", "swirlDirection"]), pixelActions: freeze([129, 130]), limitation: "Pixel-access implementation is opaque; deterministic polar swirl preserves the exposed state model." },
  ({ random }) => ({ amount: .2 + random() * .8, direction: random() < .5 ? -1 : 1 }),
  (pixels, width, height, { amount, direction }) => {
    const output = new Uint8ClampedArray(pixels.length), cx = width / 2, cy = height / 2, radius = Math.hypot(cx, cy);
    for (let y = 0; y < height; y += 1) for (let x = 0; x < width; x += 1) {
      const dx = x - cx, dy = y - cy, distance = Math.hypot(dx, dy), angle = Math.atan2(dy, dx) + direction * amount * (1 - distance / radius);
      const sx = Math.round(cx + Math.cos(angle) * distance), sy = Math.round(cy + Math.sin(angle) * distance);
      if (sx >= 0 && sx < width && sy >= 0 && sy < height) copyPixel(pixels, output, sx, sy, x, y, width);
    }
    return output;
  });

export const lightBumpTransform = contract("light-bump", "Light Bump", "reconstructed-plugin-equivalent",
  { variables: freeze(["brightnessShift", "direction"]), limitation: "Export exposes state and timing but not the numeric plugin's exact luminance operator." },
  ({ random }) => ({ shift: 12 + Math.floor(random() * 53), direction: random() < .5 ? -1 : 1 }),
  (pixels, width, height, { shift, direction }) => mapPixels(pixels, (output, index) => {
    for (let channel = 0; channel < 3; channel += 1) output[index + channel] = clampByte(output[index + channel] + shift * direction);
  }));

export const recoveredConstructTransforms = freeze([
  mirrorTransform, flipTransform, invertTransform, saturateTransform, contrastTransform,
  scrollTransform, zoomTransform, recurseTransform, spinTransform, turnTransform,
  sharpenTransform, quicksandTransform, lightBumpTransform,
]);
