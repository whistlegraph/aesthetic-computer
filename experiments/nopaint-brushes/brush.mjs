// Portable brush reference interpreter. No DOM, AC runtime, or ambient state.
const fail = message => { throw new Error(message); };
const integer = (n, low, high) => {
  if (!Number.isInteger(n) || n < low || n > high) fail(`Expected integer ${low}…${high}`);
  return n;
};
const wrap = (n, size) => ((n % size) + size) % size;
const roundDivide = (n, d) => Math.floor((n + Math.floor(d / 2)) / d);

export function validateBrush(invocation) {
  const { brush, seed, tick } = invocation;
  integer(seed, 0, 0xffffffff);
  integer(tick, 0, 3600);
  if (brush.schema !== "ac-brush" || brush.version !== 1 ||
      typeof brush.id !== "string" || !brush.id.length || brush.id.length > 128 ||
      !Array.isArray(brush.operations) || !brush.operations.length || brush.operations.length > 32)
    fail("Unsupported brush");
  if (invocation.gesture !== undefined) {
    if (!Array.isArray(invocation.gesture) || !invocation.gesture.length || invocation.gesture.length > 128) fail("Invalid gesture");
    let previous = -1;
    for (const sample of invocation.gesture) {
      integer(sample.x, 0, 255); integer(sample.y, 0, 255); integer(sample.tick, 0, 3600);
      if (sample.tick <= previous) fail("Gesture times must increase");
      previous = sample.tick;
    }
  }
  for (const op of brush.operations) {
    if (op.op === "invert") {
      if (op.amount !== undefined) integer(op.amount, 0, 255);
      if (op.durationTicks !== undefined) integer(op.durationTicks, 1, 3600);
      continue;
    }
    if (op.op === "stroke") {
      if (!invocation.gesture) fail("Stroke requires gesture");
      if (!Array.isArray(op.color) || op.color.length !== 4) fail("Invalid RGBA");
      op.color.forEach(c => integer(c, 0, 255)); integer(op.radius, 0, 16);
      continue;
    }
    if (!["path", "walk"].includes(op.op)) fail("Unsupported operation");
    if (op.op === "walk") integer(op.steps, 2, 128);
    else {
      if (!Array.isArray(op.points) || !op.points.length || op.points.length > 128) fail("Invalid path");
      for (const p of op.points) { integer(p.x, 0, 255); integer(p.y, 0, 255); }
    }
    if (!Array.isArray(op.color) || op.color.length !== 4) fail("Invalid RGBA");
    op.color.forEach(c => integer(c, 0, 255));
    integer(op.radius, 0, 16);
    integer(op.durationTicks, 1, 3600);
    integer(op.jitter, 0, 16);
    integer(op.drift.x, -8, 8); integer(op.drift.y, -8, 8);
  }
}

// Returns a fresh complete composite, never a mutation of the accepted canvas.
export function renderBrush(base, width, height, invocation) {
  integer(width, 1, 256); integer(height, 1, 256);
  if (base.length !== width * height * 4) fail("Invalid canvas");
  for (const byte of base) integer(byte, 0, 255);
  validateBrush(invocation);
  const pixels = Uint8Array.from(base);
  let seed = invocation.seed;
  let budget = 2_000_000;
  const spend = () => { if (--budget < 0) fail("Brush work limit exceeded"); };
  const random = range => {
    seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0;
    return seed % range;
  };
  function stamp(x, y, radius, color) {
    for (let dy = -radius; dy <= radius; dy++) for (let dx = -radius; dx <= radius; dx++) {
      spend();
      const px = x + dx, py = y + dy;
      if (dx * dx + dy * dy > radius * radius || px < 0 || py < 0 || px >= width || py >= height) continue;
      const i = (py * width + px) * 4;
      const sa = color[3], da = pixels[i + 3];
      const alpha = sa * 255 + da * (255 - sa);
      if (!alpha) continue;
      for (let c = 0; c < 3; c++) pixels[i + c] = roundDivide(
        color[c] * sa * 255 + pixels[i + c] * da * (255 - sa), alpha);
      pixels[i + 3] = roundDivide(alpha, 255);
    }
  }
  for (const op of invocation.brush.operations) {
    if (op.op === "invert") {
      const duration = op.durationTicks ?? 1;
      const amount = op.durationTicks === undefined ? (op.amount ?? 255) : roundDivide((op.amount ?? 255) * Math.min(invocation.tick, duration), duration);
      for (let i = 0; i < pixels.length; i += 4) {
        spend();
        for (let c = 0; c < 3; c++) pixels[i + c] = roundDivide(pixels[i + c] * (255 - amount) + (255 - pixels[i + c]) * amount, 255);
      }
      continue;
    }
    if (op.op === "stroke") {
      const samples = invocation.gesture, first = samples[0];
      if (invocation.tick < first.tick) continue;
      stamp(first.x, first.y, op.radius, op.color);
      for (let index = 1; index < samples.length; index++) {
        const previous = samples[index - 1], target = samples[index];
        if (invocation.tick <= previous.tick) break;
        let {x, y} = previous;
        const dx = Math.abs(target.x - x), dy = -Math.abs(target.y - y);
        const sx = x < target.x ? 1 : -1, sy = y < target.y ? 1 : -1;
        let error = dx + dy;
        const elapsed = Math.min(invocation.tick, target.tick) - previous.tick;
        const length = Math.floor(Math.max(dx, -dy) * elapsed / (target.tick - previous.tick));
        for (let step = 0; step < length; step++) {
          const e2 = 2 * error;
          if (e2 >= dy) { error += dy; x += sx; }
          if (e2 <= dx) { error += dx; y += sy; }
          stamp(x, y, op.radius, op.color);
        }
        if (invocation.tick < target.tick) break;
      }
      continue;
    }
    let sourcePoints = op.points;
    if (op.op === "walk") {
      let x = random(width), y = random(height), vx = random(7) - 3, vy = random(7) - 3;
      sourcePoints = [{ x, y }];
      for (let i = 1; i < op.steps; i++) {
        vx = Math.max(-3, Math.min(3, vx + random(3) - 1));
        vy = Math.max(-3, Math.min(3, vy + random(3) - 1));
        if (vx === 0 && vy === 0) vx = 1;
        if (x + vx < 0 || x + vx >= width) vx = -vx;
        if (y + vy < 0 || y + vy >= height) vy = -vy;
        x = Math.max(0, Math.min(width - 1, x + vx));
        y = Math.max(0, Math.min(height - 1, y + vy));
        sourcePoints.push({ x, y });
      }
    }
    const points = sourcePoints.map(p => ({
      x: wrap(p.x + invocation.tick * op.drift.x + random(2 * op.jitter + 1) - op.jitter, width),
      y: wrap(p.y + invocation.tick * op.drift.y + random(2 * op.jitter + 1) - op.jitter, height),
    }));
    const duration = op.durationTicks;
    const progress = (points.length - 1) * Math.min(invocation.tick, duration);
    const complete = Math.floor(progress / duration), remainder = progress % duration;
    const count = 1 + complete + (remainder > 0 ? 1 : 0);
    stamp(points[0].x, points[0].y, op.radius, op.color);
    for (let i = 1; i < count; i++) {
      let { x, y } = points[i - 1];
      const target = points[i];
      const end = i > complete ? {
        x: roundDivide(x * (op.durationTicks - remainder) + target.x * remainder, op.durationTicks),
        y: roundDivide(y * (op.durationTicks - remainder) + target.y * remainder, op.durationTicks),
      } : target;
      const dx = Math.abs(end.x - x), dy = -Math.abs(end.y - y);
      const sx = x < end.x ? 1 : -1, sy = y < end.y ? 1 : -1;
      let error = dx + dy;
      while (x !== end.x || y !== end.y) {
        const e2 = 2 * error;
        if (e2 >= dy) { error += dy; x += sx; }
        if (e2 <= dx) { error += dx; y += sy; }
        stamp(x, y, op.radius, op.color);
      }
    }
  }
  return pixels;
}

export function renderDocument(document, preview) {
  if (document.schema !== "ac-painting-prototype" || document.version !== 1) fail("Unsupported document");
  const { width, height, base, steps, cursor } = document;
  integer(width, 1, 256); integer(height, 1, 256);
  if (!Array.isArray(base) || base.length !== width * height * 4) fail("Invalid canvas");
  base.forEach(b => integer(b, 0, 255));
  if (!Array.isArray(steps) || steps.length > 64) fail("Invalid history");
  integer(cursor, 0, steps.length);
  steps.forEach(validateBrush); // Validate redo history too.
  let pixels = Uint8Array.from(base);
  for (const step of steps.slice(0, cursor)) pixels = renderBrush(pixels, width, height, step);
  return preview ? renderBrush(pixels, width, height, preview) : pixels;
}
