import { hash } from "./canonical.mjs";
import { Prng } from "./prng.mjs";
import { GROOVE_LAYOUT, HARDWARE_PROFILES, hardwareProfile, grooveHex, inspectPixelGroove, startPixelGroove } from "./pixel-groove.mjs";

const ALGORITHMS = Object.freeze(["bubble", "insertion", "selection", "merge", "quick", "heap"]);
const MAX_SOURCE = 512;
const MAX_TRACE = 2048;
const MAX_OPS = 50_000_000;
const GRID = Object.freeze({ width: 12, height: 8 });
const BASE_RASTER = Object.freeze({
  width: 128, height: 128, channels: 3, pixels: 128 * 128,
  bytes: 128 * 128 * 3, maxStages: 8, bytecodeStride: 24,
  permaMargin: 16, permaCells: 160 * 160 - 128 * 128,
  permaCellBytes: 3, permaBytes: GROOVE_LAYOUT.bytes,
  marginCoreCells: GROOVE_LAYOUT.tracks.state.base, spriteSlots: 4, spriteSize: 32,
  spriteCells: 1 + 32 * 32,
  marginProtectedCells: GROOVE_LAYOUT.tracks.source.base + GROOVE_LAYOUT.tracks.source.pixels,
});
let RASTER = BASE_RASTER;
const PROFILE_LIST = Object.freeze(Object.values(HARDWARE_PROFILES));
const RASTER_CAPACITY = GRID.width * GRID.height * 4 * 4 * 4 * PROFILE_LIST.length;

function rasterGeometry(profile) {
  const machine = hardwareProfile(profile);
  return Object.freeze({ ...BASE_RASTER,
    width: machine.resolution, height: machine.resolution,
    pixels: machine.resolution ** 2, bytes: machine.resolution ** 2 * BASE_RASTER.channels,
  });
}

function scaleRasterPlan(plan, profile) {
  const machine = hardwareProfile(profile);
  if (machine.resolution === BASE_RASTER.width) return plan;
  const coordinate = (value) => Math.max(0, Math.min(machine.resolution - 1,
    Math.round(value * (machine.resolution - 1) / (BASE_RASTER.width - 1))));
  const extent = (value) => Math.max(1, Math.round(value * machine.resolution / BASE_RASTER.width));
  const offset = (value) => value === 0 ? 0 : Math.sign(value) * Math.max(1, Math.round(Math.abs(value) * machine.scale));
  const operations = plan.operations.map(({ name, args }) => {
    const next = [...args];
    if (name === "line") for (const index of [0, 1, 2, 3]) next[index] = coordinate(next[index]);
    else if (name === "triangle") for (const index of [0, 1, 2, 3, 4, 5]) next[index] = coordinate(next[index]);
    else if (name === "flood") for (const index of [0, 1]) next[index] = coordinate(next[index]);
    else if (name === "box") {
      next[0] = coordinate(next[0]); next[1] = coordinate(next[1]);
      next[2] = Math.min(machine.resolution - next[0], extent(next[2]));
      next[3] = Math.min(machine.resolution - next[1], extent(next[3]));
    } else if (name === "copy") {
      next[0] = coordinate(next[0]); next[1] = coordinate(next[1]);
      next[2] = Math.min(BASE_RASTER.spriteSize, machine.resolution - next[0], extent(next[2]));
      next[3] = Math.min(BASE_RASTER.spriteSize, machine.resolution - next[1], extent(next[3]));
    } else if (name === "paste") {
      next[1] = coordinate(next[1]); next[2] = coordinate(next[2]);
    } else if (name === "shift" || name === "mix") {
      next[0] = offset(next[0]); next[1] = offset(next[1]);
    }
    return Object.freeze({ name, args: Object.freeze(next) });
  });
  return Object.freeze({ ...plan, operations: Object.freeze(operations) });
}

function round(value, places = 6) {
  const scale = 10 ** places;
  return Math.round(value * scale) / scale;
}

function clamp(value, low = 0, high = 1) {
  return Math.max(low, Math.min(high, value));
}

export function tokenize(source) {
  const text = String(source || "").trim();
  if (!text || text.length > MAX_SOURCE) throw new TypeError("Lisp source must be 1..512 characters");
  const tokens = text.match(/[()]|[^\s()]+/g) || [];
  if (tokens.join("").length !== text.replace(/\s/g, "").length) throw new TypeError("unreadable Lisp source");
  return tokens;
}

export function readLisp(source) {
  const tokens = tokenize(source);
  let cursor = 0;
  function read() {
    const token = tokens[cursor++];
    if (token === undefined) throw new TypeError("unexpected end of Lisp source");
    if (token === ")") throw new TypeError("unexpected )");
    if (token !== "(") {
      if (/^-?\d+$/.test(token)) return Number(token);
      if (!/^[a-z][a-z0-9._/+:-]*$/i.test(token)) throw new TypeError(`invalid Lisp symbol: ${token}`);
      return token.toLowerCase();
    }
    const form = [];
    while (tokens[cursor] !== ")") {
      if (cursor >= tokens.length) throw new TypeError("unclosed Lisp form");
      form.push(read());
    }
    cursor += 1;
    return form;
  }
  const form = read();
  if (cursor !== tokens.length) throw new TypeError("Lisp source must contain one form");
  return form;
}

export function compileSortLisp(source) {
  const form = readLisp(source);
  if (!Array.isArray(form)) throw new TypeError("sort program must be a list");
  if (form[0] === "sort" && form.length === 2 && ALGORITHMS.includes(form[1])) {
    return Object.freeze({ kind: "sort", algorithm: form[1], source: `(sort ${form[1]})` });
  }
  if (
    form[0] === "hybrid" && form.length === 4 && Number.isInteger(form[1]) &&
    form[1] >= 2 && form[1] <= 64 && ALGORITHMS.includes(form[2]) && ALGORITHMS.includes(form[3])
  ) {
    return Object.freeze({
      kind: "hybrid",
      threshold: form[1],
      small: form[2],
      large: form[3],
      source: `(hybrid ${form[1]} ${form[2]} ${form[3]})`,
    });
  }
  throw new TypeError("program must be (sort algorithm) or (hybrid 2..64 small large)");
}

function canonicalForm(form) {
  return Array.isArray(form) ? `(${form.map(canonicalForm).join(" ")})` : String(form);
}

function integerBetween(value, low, high) {
  return Number.isInteger(value) && value >= low && value <= high;
}

export function compileRasterLisp(source) {
  const form = readLisp(source);
  if (!Array.isArray(form) || form[0] !== "raster" || form.length < 2 || form.length > RASTER.maxStages + 1) {
    throw new TypeError("raster program must be (raster op...) with 1..8 stages");
  }
  const operations = form.slice(1).map((op) => {
    if (!Array.isArray(op) || typeof op[0] !== "string") throw new TypeError("raster stages must be lists");
    const [name, ...args] = op;
    const valid =
      (name === "add" && args.length === 3 && args.every((v) => integerBetween(v, -255, 255))) ||
      (name === "xor" && args.length === 3 && args.every((v) => integerBetween(v, 0, 255))) ||
      (["and", "or"].includes(name) && args.length === 3 && args.every((v) => integerBetween(v, 0, 255))) ||
      (name === "shift" && args.length === 2 && args.every((v) => integerBetween(v, -4, 4))) ||
      (name === "mix" && args.length === 3 && integerBetween(args[0], -4, 4) && integerBetween(args[1], -4, 4) && integerBetween(args[2], 0, 255)) ||
      (name === "solarize" && args.length === 1 && integerBetween(args[0], 0, 255)) ||
      (name === "blur" && args.length === 0) ||
      (name === "edges" && args.length === 0) ||
      (name === "rotate" && args.length === 0) ||
      (name === "mirror" && args.length === 1 && ["x", "y"].includes(args[0])) ||
      (name === "channels" && args.length === 1 && ["rgb", "rbg", "grb", "gbr", "brg", "bgr"].includes(args[0]));
    const fieldRule = name === "cellular" && args.length === 2 && args.every((v) => integerBetween(v, 0, 511));
    const point = (offset) => integerBetween(args[offset], 0, RASTER.width - 1) && integerBetween(args[offset + 1], 0, RASTER.height - 1);
    const color = (offset) => args.slice(offset, offset + 3).length === 3 && args.slice(offset, offset + 3).every((v) => integerBetween(v, 0, 255));
    const geometric =
      (name === "line" && args.length === 7 && point(0) && point(2) && color(4)) ||
      (name === "triangle" && args.length === 9 && point(0) && point(2) && point(4) && color(6)) ||
      (name === "flood" && args.length === 6 && point(0) && integerBetween(args[2], 0, 255) && color(3)) ||
      (name === "box" && args.length === 6 && point(0) && integerBetween(args[2], 4, RASTER.width - args[0]) &&
        integerBetween(args[3], 4, RASTER.height - args[1]) && integerBetween(args[4], 0, 255) && integerBetween(args[5], 0, 3)) ||
      (name === "copy" && args.length === 5 && point(0) && integerBetween(args[2], 1, Math.min(32, RASTER.width - args[0])) &&
        integerBetween(args[3], 1, Math.min(32, RASTER.height - args[1])) && integerBetween(args[4], 0, 3)) ||
      (name === "paste" && args.length === 4 && integerBetween(args[0], 0, 3) && point(1) &&
        ["replace", "xor", "add", "mask"].includes(args[3]));
    if (!valid && !geometric && !fieldRule) throw new TypeError(`invalid bounded raster stage: ${canonicalForm(op)}`);
    return Object.freeze({ name, args: Object.freeze([...args]) });
  });
  return Object.freeze({ kind: "raster", operations: Object.freeze(operations), source: canonicalForm(form) });
}

function rasterInput(seed, requestedMode = null) {
  const rng = new Prng(seed);
  const mode = requestedMode ?? Math.floor(rng.float() * 6);
  const field = new Uint8Array(RASTER.bytes);
  for (let y = 0; y < RASTER.height; y += 1) for (let x = 0; x < RASTER.width; x += 1) {
    const at = (y * RASTER.width + x) * 3;
    const nx = x * BASE_RASTER.width / RASTER.width;
    const ny = y * BASE_RASTER.height / RASTER.height;
    const jitter = Math.floor(rng.float() * 9) - 4;
    const distance = Math.hypot(nx - BASE_RASTER.width / 2, ny - BASE_RASTER.height / 2);
    const radial = distance < BASE_RASTER.width * .28 ? 32 : 0;
    const checker = ((nx >> 4) + (ny >> 4)) & 1 ? 16 : -16;
    let r, g, b;
    if (mode === 0) [r, g, b] = [nx * 2 + radial, ny * 2 + checker, nx + ny + radial - checker];
    else if (mode === 1) {
      const rings = (Math.floor(distance / 9) & 1) * 176;
      [r, g, b] = [rings + nx / 2, 224 - rings + ny / 3, 64 + rings / 2];
    } else if (mode === 2) {
      const impulse = ((nx - 24) ** 2 + (ny - 38) ** 2 < 144) || ((nx - 91) ** 2 + (ny - 84) ** 2 < 324);
      [r, g, b] = impulse ? [224, 128 + checker, 64] : [16 + nx / 4, 24 + ny / 5, 32 + radial];
    } else if (mode === 3) {
      const wave = 128 + 72 * Math.sin(nx / 13) + 48 * Math.cos(ny / 17);
      [r, g, b] = [wave, 255 - wave / 2, 64 + 64 * Math.sin((nx + ny) / 21)];
    } else if (mode === 4) {
      const strata = ((nx + ny * 2) >> 3) & 1 ? 196 : 44;
      [r, g, b] = [strata, strata * .72 + radial, 236 - strata / 2];
    } else {
      const cell = (((nx >> 5) * 53 + (ny >> 5) * 97 + 41) & 255);
      [r, g, b] = [cell + nx / 3, cell * .65 + ny / 2, 224 - cell / 2 + radial];
    }
    field[at] = Math.max(0, Math.min(255, ((r + jitter) >> 4) << 4));
    field[at + 1] = Math.max(0, Math.min(255, ((g + jitter) >> 4) << 4));
    field[at + 2] = Math.max(0, Math.min(255, ((b + jitter) >> 4) << 4));
  }
  return field;
}

function rasterIndex(x, y, channel = 0) {
  const xx = (x + RASTER.width) % RASTER.width;
  const yy = (y + RASTER.height) % RASTER.height;
  return (yy * RASTER.width + xx) * 3 + channel;
}

function energyOf(previous, output) {
  let change = 0, potential = 0, sum = 0, sumSquares = 0, spatial = 0, deltaNoise = 0, chromaNoise = 0, colorfulness = 0, midtones = 0;
  for (let i = 0; i < output.length; i += 1) {
    change += Math.abs(output[i] - previous[i]) / 255;
    potential += Math.max(previous[i], 255 - previous[i]) / 255;
    sum += output[i]; sumSquares += output[i] * output[i];
    if (output[i] >= 64 && output[i] <= 192) midtones += 1;
  }
  const mean = sum / output.length;
  const variance = Math.max(0, sumSquares / output.length - mean * mean) / 16256.25;
  for (let y = 0; y < RASTER.height; y += 1) for (let x = 0; x < RASTER.width; x += 1)
    for (let channel = 0; channel < 3; channel += 1) {
      const value = output[rasterIndex(x, y, channel)];
      spatial += Math.abs(value - output[rasterIndex(x + 1, y, channel)]) / 255;
      spatial += Math.abs(value - output[rasterIndex(x, y + 1, channel)]) / 255;
      const delta = output[rasterIndex(x, y, channel)] - previous[rasterIndex(x, y, channel)];
      const deltaX = output[rasterIndex(x + 1, y, channel)] - previous[rasterIndex(x + 1, y, channel)];
      const deltaY = output[rasterIndex(x, y + 1, channel)] - previous[rasterIndex(x, y + 1, channel)];
      deltaNoise += (Math.abs(delta - deltaX) + Math.abs(delta - deltaY)) / 510;
      if (channel === 0) {
        const at = rasterIndex(x, y), right = rasterIndex(x + 1, y);
        colorfulness += (Math.abs(output[at] - output[at + 1]) + Math.abs(output[at + 2] - output[at + 1])) / 510;
        chromaNoise += (Math.abs((output[at] - output[at + 1]) - (output[right] - output[right + 1]))
          + Math.abs((output[at + 2] - output[at + 1]) - (output[right + 2] - output[right + 1]))) / 510;
      }
    }
  const actual = change / output.length;
  const available = potential / output.length;
  const spatialEnergy = spatial / (RASTER.pixels * 6);
  const activation = actual / Math.max(1e-9, available);
  const noise = clamp(deltaNoise / (RASTER.pixels * 6) * .72 + chromaNoise / RASTER.pixels * .28);
  const coherence = actual / Math.max(1e-9, actual + noise);
  const muddiness = clamp(midtones / output.length * (1 - clamp(spatialEnergy * 3)) * (1 - clamp(Math.sqrt(variance) * 1.5)));
  const state = variance < .002 && spatialEnergy < .002 ? "collapsed" : activation < .0005 ? "dormant" : "alive";
  return { actual: round(actual), potential: round(available), activation: round(activation), variance: round(clamp(variance)), spatial: round(clamp(spatialEnergy)), noise: round(noise), coherence: round(coherence), muddiness: round(muddiness), colorfulness: round(colorfulness / RASTER.pixels), state };
}

function dynamicTags(energy, aliveness) {
  const last = energy.at(-1) || {};
  const category = aliveness === "collapsed" ? "collapse" : aliveness === "dormant" ? "still" :
    last.noise > .24 && last.coherence < .42 ? "chaos" : last.spatial > .3 ? "edge" :
    last.variance > .18 && last.coherence > .52 ? "volume" : last.actual > .07 ? "pulse" : "flow";
  const tags = [category];
  if ((last.colorfulness || 0) < .04 && (last.variance || 0) < .08) tags.push("gray-wash");
  if ((last.noise || 0) > .3) tags.push("raw-buffer");
  if ((last.muddiness || 0) > .45) tags.push("muddy");
  if (last.coherence > .58) tags.push("coherent");
  if (last.actual > .08) tags.push("high-difference");
  if (last.noise < .12) tags.push("low-noise");
  return tags;
}

function runRasterPlan(plan, input, { captureFrames = false } = {}) {
  let current = Uint8Array.from(input);
  let next = new Uint8Array(RASTER.bytes);
  const queue = new Uint32Array(RASTER.pixels);
  const visited = new Uint8Array(RASTER.pixels);
  const sanctuary = new Uint8Array(RASTER.pixels);
  const permaBytes = new Uint8Array(RASTER.permaBytes);
  const permastore = new Uint32Array(permaBytes.buffer);
  permastore[0] = plan.operations.length; // machine header; RGB stages have no write authority here.
  const spriteStride = RASTER.spriteCells * RASTER.permaCellBytes;
  const sprites = Array.from({ length: RASTER.spriteSlots }, (_, slot) => ({
    width: 0, height: 0,
    // The JS verifier uses views into the same bounded permamargin allocation.
    pixels: new Uint8Array(permaBytes.buffer,
      (GROOVE_LAYOUT.tracks.sprites.base * RASTER.permaCellBytes) + slot * spriteStride + RASTER.permaCellBytes,
      RASTER.spriteSize * RASTER.spriteSize * RASTER.channels),
  }));
  for (const { name, args } of plan.operations) if (name === "box") {
    const [bx, by, width, height] = args;
    for (let y = by; y < by + height; y += 1) for (let x = bx; x < bx + width; x += 1)
      sanctuary[y * RASTER.width + x] = 1;
  }
  const metrics = {
    reads: 0, writes: 0, operations: 0, stages: plan.operations.length,
    allocationBytes: RASTER.bytes * 2 + queue.byteLength + visited.byteLength + sanctuary.byteLength + permastore.byteLength,
    energy: [],
  };
  const frames = captureFrames ? [Buffer.from(current).toString("hex")] : [];
  const read = (buffer, at) => { metrics.reads += 1; metrics.operations += 1; return buffer[at]; };
  const write = (buffer, at, value) => {
    metrics.writes += 1; metrics.operations += 1;
    if (metrics.operations > MAX_OPS) throw new Error("raster program exceeded operation budget");
    buffer[at] = Math.max(0, Math.min(255, Math.round(value)));
  };
  const copyField = () => {
    next.set(current); metrics.reads += RASTER.bytes; metrics.writes += RASTER.bytes; metrics.operations += RASTER.bytes * 2;
  };
  const paintPixel = (x, y, color) => {
    if (x < 0 || x >= RASTER.width || y < 0 || y >= RASTER.height) return;
    const at = rasterIndex(x, y);
    for (let channel = 0; channel < 3; channel += 1) write(next, at + channel, color[channel]);
  };
  const paintLine = (x0, y0, x1, y1, color) => {
    let dx = Math.abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
    let dy = -Math.abs(y1 - y0), sy = y0 < y1 ? 1 : -1, error = dx + dy;
    for (;;) {
      paintPixel(x0, y0, color); if (x0 === x1 && y0 === y1) break;
      const twice = error * 2;
      if (twice >= dy) { error += dy; x0 += sx; }
      if (twice <= dx) { error += dx; y0 += sy; }
    }
  };
  for (const { name, args } of plan.operations) {
    const before = current;
    if (name === "line") {
      copyField(); paintLine(...args.slice(0, 4), args.slice(4));
    } else if (name === "triangle") {
      copyField();
      const [x0, y0, x1, y1, x2, y2, r, g, b] = args;
      paintLine(x0, y0, x1, y1, [r, g, b]); paintLine(x1, y1, x2, y2, [r, g, b]); paintLine(x2, y2, x0, y0, [r, g, b]);
    } else if (name === "flood") {
      copyField(); visited.fill(0);
      const [startX, startY, tolerance, r, g, b] = args;
      const targetAt = rasterIndex(startX, startY), target = [current[targetAt], current[targetAt + 1], current[targetAt + 2]];
      let head = 0, tail = 0; queue[tail++] = startY * RASTER.width + startX; visited[startY * RASTER.width + startX] = 1;
      while (head < tail) {
        const pixel = queue[head++], x = pixel % RASTER.width, y = Math.floor(pixel / RASTER.width), at = rasterIndex(x, y);
        metrics.reads += 3; metrics.operations += 3;
        if (Math.max(Math.abs(current[at] - target[0]), Math.abs(current[at + 1] - target[1]), Math.abs(current[at + 2] - target[2])) > tolerance) continue;
        paintPixel(x, y, [r, g, b]);
        for (const [nx, ny] of [[x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1]]) {
          if (nx < 0 || nx >= RASTER.width || ny < 0 || ny >= RASTER.height) continue;
          const key = ny * RASTER.width + nx;
          if (!visited[key]) { visited[key] = 1; queue[tail++] = key; }
        }
      }
    } else if (name === "copy") {
      copyField();
      const [sourceX, sourceY, width, height, slot] = args;
      const sprite = sprites[slot]; sprite.width = width; sprite.height = height; sprite.pixels.fill(0);
      for (let y = 0; y < height; y += 1) for (let x = 0; x < width; x += 1) for (let channel = 0; channel < 3; channel += 1) {
        sprite.pixels[(y * 32 + x) * 3 + channel] = read(current, rasterIndex(sourceX + x, sourceY + y, channel));
        metrics.writes += 1; metrics.operations += 1;
      }
    } else if (name === "paste") {
      copyField();
      const [slot, targetX, targetY, mode] = args, sprite = sprites[slot];
      for (let y = 0; y < sprite.height; y += 1) for (let x = 0; x < sprite.width; x += 1) {
        if (targetX + x >= RASTER.width || targetY + y >= RASTER.height) continue;
        for (let channel = 0; channel < 3; channel += 1) {
          const at = rasterIndex(targetX + x, targetY + y, channel);
          const source = sprite.pixels[(y * 32 + x) * 3 + channel];
          const destination = read(current, at);
          const value = mode === "xor" ? destination ^ source : mode === "add" ? destination + source :
            mode === "mask" ? (source > 127 ? source : destination) : source;
          write(next, at, value);
        }
      }
    } else if (name === "cellular") {
      const [birthMask, survivalMask] = args;
      for (let y = 0; y < RASTER.height; y += 1) for (let x = 0; x < RASTER.width; x += 1) {
        let neighbors = 0;
        const sums = [0, 0, 0];
        for (let oy = -1; oy <= 1; oy += 1) for (let ox = -1; ox <= 1; ox += 1) {
          if (ox === 0 && oy === 0) continue;
          const at = rasterIndex(x + ox, y + oy);
          const rgb = [read(current, at), read(current, at + 1), read(current, at + 2)];
          sums[0] += rgb[0]; sums[1] += rgb[1]; sums[2] += rgb[2];
          if ((rgb[0] + rgb[1] + rgb[2]) / 3 >= 128) neighbors += 1;
        }
        const at = rasterIndex(x, y);
        const self = [read(current, at), read(current, at + 1), read(current, at + 2)];
        const alive = (self[0] + self[1] + self[2]) / 3 >= 128;
        const nextAlive = Boolean((alive ? survivalMask : birthMask) & (1 << neighbors));
        for (let channel = 0; channel < 3; channel += 1)
          write(next, at + channel, nextAlive ? Math.min(255, sums[channel] / 8 + 24) : self[channel] * .18);
      }
    } else if (name === "box") {
      copyField();
      const [bx, by, width, height, permeability, rule] = args;
      const amount = permeability / 255;
      const inside = (x, y, channel) => read(current, rasterIndex(
        Math.max(bx, Math.min(bx + width - 1, x)),
        Math.max(by, Math.min(by + height - 1, y)), channel));
      for (let y = by; y < by + height; y += 1) for (let x = bx; x < bx + width; x += 1)
        for (let channel = 0; channel < 3; channel += 1) {
          const boundary = x === bx || x === bx + width - 1 || y === by || y === by + height - 1;
          let value;
          if (boundary) {
            const ox = x === bx ? x - 1 : x === bx + width - 1 ? x + 1 : x;
            const oy = y === by ? y - 1 : y === by + height - 1 ? y + 1 : y;
            value = inside(x, y, channel) * (1 - amount) + read(current, rasterIndex(ox, oy, channel)) * amount;
          } else if (rule === 0) {
            value = (inside(x - 1, y, channel) + inside(x + 1, y, channel) + inside(x, y - 1, channel) + inside(x, y + 1, channel)) / 4;
          } else if (rule === 1) {
            value = inside(x - 1, y, channel) ^ inside(x + 1, y, channel) ^ inside(x, y - 1, channel);
          } else if (rule === 2) {
            const localX = x - bx, localY = y - by;
            value = inside(bx + Math.floor(localY * width / height), by + height - 1 - Math.floor(localX * height / width), channel);
          } else {
            const neighborhood = inside(x - 1, y, channel) + inside(x + 1, y, channel) + inside(x, y - 1, channel) + inside(x, y + 1, channel);
            value = neighborhood / 4 >= inside(x, y, channel) ? 224 : 24;
          }
          write(next, rasterIndex(x, y, channel), value);
        }
    } else {
      for (let y = 0; y < RASTER.height; y += 1) for (let x = 0; x < RASTER.width; x += 1) {
        for (let channel = 0; channel < 3; channel += 1) {
          const at = rasterIndex(x, y, channel);
          let value;
          if (name === "add") value = read(current, at) + args[channel];
          else if (name === "xor") value = read(current, at) ^ args[channel];
          else if (name === "and") value = read(current, at) & args[channel];
          else if (name === "or") value = read(current, at) | args[channel];
          else if (name === "shift") value = read(current, rasterIndex(x - args[0], y - args[1], channel));
          else if (name === "mix") {
            const amount = args[2] / 255;
            value = read(current, at) * (1 - amount) + read(current, rasterIndex(x + args[0], y + args[1], channel)) * amount;
          } else if (name === "solarize") {
            const original = read(current, at); value = original >= args[0] ? 255 - original : original;
          } else if (name === "blur") {
            value = 0;
            for (let oy = -1; oy <= 1; oy += 1) for (let ox = -1; ox <= 1; ox += 1)
              value += read(current, rasterIndex(x + ox, y + oy, channel)) / 9;
          } else if (name === "edges") {
            const original = read(current, at);
            value = Math.abs(original - read(current, rasterIndex(x + 1, y, channel))) + Math.abs(original - read(current, rasterIndex(x, y + 1, channel)));
          } else if (name === "rotate") value = read(current, rasterIndex(y, RASTER.width - 1 - x, channel));
          else if (name === "mirror") value = read(current, rasterIndex(args[0] === "x" ? RASTER.width - 1 - x : x, args[0] === "y" ? RASTER.height - 1 - y : y, channel));
          else {
            const order = args[0]; value = read(current, rasterIndex(x, y, "rgb".indexOf(order[channel])));
          }
          write(next, at, value);
        }
      }
    }
    if (name !== "box") for (let pixel = 0; pixel < RASTER.pixels; pixel += 1) if (sanctuary[pixel])
      for (let channel = 0; channel < 3; channel += 1) write(next, pixel * 3 + channel, read(current, pixel * 3 + channel));
    if (metrics.operations > MAX_OPS) throw new Error("raster program exceeded operation budget");
    metrics.energy.push({ op: name, abstract: name === "copy", ...energyOf(before, next) });
    [current, next] = [next, current]; next.fill(0);
    if (captureFrames) frames.push(Buffer.from(current).toString("hex"));
  }
  return { values: current, metrics, frames };
}

function rasterBytecode(plan) {
  const opcodes = { add: 1, xor: 2, shift: 3, mix: 4, solarize: 5, blur: 6, edges: 7, rotate: 8, mirror: 9, channels: 10, and: 11, or: 12, line: 13, triangle: 14, flood: 15, box: 16, copy: 17, paste: 18, cellular: 19 };
  const channelOrders = ["rgb", "rbg", "grb", "gbr", "brg", "bgr"];
  const bytes = Buffer.alloc(plan.operations.length * RASTER.bytecodeStride);
  plan.operations.forEach(({ name, args }, index) => {
    const offset = index * RASTER.bytecodeStride;
    bytes[offset] = opcodes[name];
    const encoded = name === "mirror" ? [args[0] === "y" ? 1 : 0] :
      name === "channels" ? [channelOrders.indexOf(args[0])] :
      name === "paste" ? [args[0], args[1], args[2], ["replace", "xor", "add", "mask"].indexOf(args[3])] : args;
    for (let arg = 0; arg < Math.min(10, encoded.length); arg += 1) bytes.writeInt16LE(encoded[arg], offset + 1 + arg * 2);
  });
  return bytes.toString("hex");
}

function rasterDescriptor(runs, inputs) {
  let displacement = 0, total = 0, edge = 0, variance = 0, colors = 0;
  for (let runIndex = 0; runIndex < runs.length; runIndex += 1) {
    const values = runs[runIndex].values;
    const input = inputs[runIndex];
    const bins = new Set();
    let mean = 0;
    for (let i = 0; i < values.length; i += 1) {
      displacement += Math.abs(values[i] - input[i]) / 255; total += 1; mean += values[i];
    }
    mean /= values.length;
    for (let y = 0; y < RASTER.height; y += 1) for (let x = 0; x < RASTER.width; x += 1) {
      const at = rasterIndex(x, y);
      bins.add(`${values[at] >> 5}:${values[at + 1] >> 5}:${values[at + 2] >> 5}`);
      for (let c = 0; c < 3; c += 1) {
        variance += ((values[at + c] - mean) / 255) ** 2;
        edge += Math.abs(values[at + c] - values[rasterIndex(x + 1, y, c)]) / 255;
        edge += Math.abs(values[at + c] - values[rasterIndex(x, y + 1, c)]) / 255;
      }
    }
    colors += bins.size / (RASTER.width * RASTER.height);
  }
  const pixels = runs.length * RASTER.width * RASTER.height;
  return [displacement / total, colors / runs.length, edge / (pixels * 6), Math.sqrt(variance / (pixels * 3)), planComplexity(runs)].map((v) => round(clamp(v)));
}

function planComplexity(runs) {
  const avg = runs.reduce((sum, run) => sum + run.metrics.operations, 0) / Math.max(1, runs.length);
  return Math.log2(1 + avg / RASTER.bytes) / 6;
}

function evaluateRasterAtProfile(source, canonicalPlan, machine, { origin = "grammar", parent = null, generation = 0,
  entropySource = "memory", authorityUtcMs = 0, entropySeed = 0, _id = null,
  reviewParent = null, reviewCriticism = null, inheritedCapability = null, mutationHints = [] } = {}) {
  const plan = scaleRasterPlan(canonicalPlan, machine.name);
  const inputs = [0, 1, 2, 3].map((n) => rasterInput(`raster-verification-${n}`, n));
  const runs = inputs.map((input) => runRasterPlan(plan, input));
  for (const run of runs) {
    if (!(run.values instanceof Uint8Array) || run.values.length !== RASTER.bytes) throw new Error("raster verification changed field bounds");
    if (run.metrics.allocationBytes !== RASTER.bytes * 2 + RASTER.pixels * 6 + RASTER.permaBytes || run.metrics.operations > MAX_OPS) throw new Error("raster verification exceeded fixed resources");
  }
  const descriptor = rasterDescriptor(runs, inputs);
  const identityProfile = machine.name === "standard" ? "" : `\nprofile:${machine.name}`;
  const id = _id || hash(`${canonicalPlan.source}\n${parent || "root"}\n${generation}${identityProfile}`).slice(0, 12);
  const bytecode = rasterBytecode(canonicalPlan);
  const grooveBytes = startPixelGroove({ id, parent, generation, source: canonicalPlan.source, bytecode,
    entropySource, authorityUtcMs, entropySeed, profile: machine.name });
  const grooveRecord = inspectPixelGroove(grooveBytes);
  const groove = grooveHex(grooveBytes);
  const displayIn = rasterInput(id);
  const sample = runRasterPlan(plan, displayIn);
  const metrics = runs.reduce((out, run) => {
    out.reads += run.metrics.reads; out.writes += run.metrics.writes; out.operations += run.metrics.operations;
    return out;
  }, { reads: 0, writes: 0, operations: 0, stages: plan.operations.length, allocationBytes: RASTER.bytes * 2 });
  const energy = sample.metrics.energy;
  const visibleEnergy = energy.filter((stage) => !stage.abstract);
  const aliveness = visibleEnergy.some((stage) => stage.state !== "alive") ? visibleEnergy.findLast((stage) => stage.state !== "alive").state : "alive";
  const tags = dynamicTags(energy, aliveness);
  const boxCount = plan.operations.filter((operation) => operation.name === "box").length;
  const spriteOps = plan.operations.filter((operation) => operation.name === "copy" || operation.name === "paste").length;
  const cellularOps = plan.operations.filter((operation) => operation.name === "cellular").length;
  if (boxCount) tags.push("boxed-computation");
  if (boxCount > 1) tags.push("nested-worlds");
  if (spriteOps) tags.push("sprite-memory");
  if (cellularOps) tags.push("cellular-field");
  if (inheritedCapability) tags.push(`capability-${inheritedCapability}`);
  tags.push(`hardware-${machine.name}`);
  const lastEnergy = energy.at(-1) || { coherence: 0, noise: 1 };
  const signalQuality = .35 + clamp(lastEnergy.coherence || 0) * .85;
  const noisePenalty = 1 / (1 + (lastEnergy.noise || 0) * 2.5);
  const mudPenalty = 1 / (1 + (lastEnergy.muddiness || 0) * 1.8);
  return {
    id, domain: "raster", source: canonicalPlan.source,
    type: `PixelGroove<160,RGB24,v1> -> RGBField<${machine.resolution},${machine.resolution},u8>`,
    plan: canonicalPlan, hardware: { ...machine, fieldBytes: RASTER.bytes,
      workingBytes: RASTER.bytes * 2 + RASTER.permaBytes, readerHz: 240, sequenceHz: 30 },
    origin, parent, generation, entropy: { source: entropySource, authorityUtcMs, seed: entropySeed >>> 0 },
    capabilityLineage: inheritedCapability ? {
      reviewParent: reviewParent || parent, capability: inheritedCapability,
      criticism: reviewCriticism ? String(reviewCriticism).slice(0, 180) : null,
      mutationHints: [...mutationHints].slice(0, 3), verifier: "bounded-js",
    } : null,
    status: "verified", engine: "bounded-js", descriptor,
    cell: [Math.min(GRID.width - 1, Math.floor(descriptor[2] * GRID.width)), Math.min(GRID.height - 1, Math.floor(descriptor[0] * GRID.height))],
    niche: [
      Math.min(GRID.width - 1, Math.floor(descriptor[2] * GRID.width)),
      Math.min(GRID.height - 1, Math.floor(descriptor[0] * GRID.height)),
      Math.min(3, Math.floor(descriptor[1] * 4)),
      Math.min(3, Math.floor(descriptor[3] * 4)),
      Math.min(3, Math.floor(descriptor[4] * 4)),
      PROFILE_LIST.findIndex((profile) => profile.name === machine.name),
    ],
    quality: round((aliveness === "alive" ? 1 : .1) * signalQuality * noisePenalty * mudPenalty * clamp(descriptor[0] * .35 + descriptor[1] * .25 + descriptor[2] * .4) / (1 + metrics.operations / 10000000)),
    aliveness, tags,
    metrics: { ...metrics, boxes: boxCount, spriteOps, cellularOps, allocationBytes: sample.metrics.allocationBytes, energy },
    sample: {
      width: RASTER.width, height: RASTER.height, rgb: Buffer.from(sample.values).toString("hex"),
      bytecode, grooveVersion: GROOVE_LAYOUT.version, grooveBytes: grooveBytes.length,
      grooveHash: grooveRecord.protectedHash, groovePc: grooveRecord.pc,
      groove, energy, frames: sample.frames,
    },
  };
}

export function evaluateRasterProgram(source, options = {}) {
  const canonicalPlan = compileRasterLisp(source);
  const machine = hardwareProfile(options.profile || "standard");
  const previous = RASTER;
  RASTER = rasterGeometry(machine.name);
  try {
    return evaluateRasterAtProfile(source, canonicalPlan, machine, options);
  } finally {
    RASTER = previous;
  }
}

function makeTrace(input) {
  const values = [...input];
  const events = [];
  const metrics = { comparisons: 0, swaps: 0, writes: 0, reads: 0, maxDepth: 0, distance: 0, operations: 0 };
  function event(op, a, b, value, depth = 0) {
    metrics.operations += 1;
    if (metrics.operations > MAX_OPS) throw new Error("program exceeded operation budget");
    metrics.maxDepth = Math.max(metrics.maxDepth, depth);
    if (events.length < MAX_TRACE) events.push([op, a, b, value]);
  }
  return {
    values,
    events,
    metrics,
    compare(a, b, depth = 0) {
      metrics.comparisons += 1;
      metrics.reads += 2;
      metrics.distance += Math.abs(a - b);
      event("c", a, b, values[a] - values[b], depth);
      return values[a] - values[b];
    },
    compareValue(index, value, depth = 0) {
      metrics.comparisons += 1;
      metrics.reads += 1;
      event("c", index, -1, values[index] - value, depth);
      return values[index] - value;
    },
    swap(a, b, depth = 0) {
      if (a === b) return;
      metrics.swaps += 1;
      metrics.reads += 2;
      metrics.writes += 2;
      metrics.distance += Math.abs(a - b);
      [values[a], values[b]] = [values[b], values[a]];
      event("s", a, b, 0, depth);
    },
    write(index, value, depth = 0) {
      metrics.writes += 1;
      values[index] = value;
      event("w", index, -1, value, depth);
    },
    read(index) {
      metrics.reads += 1;
      return values[index];
    },
  };
}

function bubble(t, lo = 0, hi = t.values.length, depth = 0) {
  for (let end = hi - 1; end > lo; end -= 1) {
    let changed = false;
    for (let i = lo; i < end; i += 1) {
      if (t.compare(i, i + 1, depth) > 0) { t.swap(i, i + 1, depth); changed = true; }
    }
    if (!changed) break;
  }
}

function insertion(t, lo = 0, hi = t.values.length, depth = 0) {
  for (let i = lo + 1; i < hi; i += 1) {
    const value = t.read(i);
    let j = i - 1;
    while (j >= lo && t.compareValue(j, value, depth) > 0) {
      t.write(j + 1, t.read(j), depth);
      j -= 1;
    }
    t.write(j + 1, value, depth);
  }
}

function selection(t, lo = 0, hi = t.values.length, depth = 0) {
  for (let i = lo; i < hi - 1; i += 1) {
    let least = i;
    for (let j = i + 1; j < hi; j += 1) if (t.compare(j, least, depth) < 0) least = j;
    t.swap(i, least, depth);
  }
}

function merge(t, lo = 0, hi = t.values.length, depth = 0) {
  if (hi - lo <= 1) return;
  const mid = lo + Math.floor((hi - lo) / 2);
  merge(t, lo, mid, depth + 1);
  merge(t, mid, hi, depth + 1);
  const left = t.values.slice(lo, mid);
  const right = t.values.slice(mid, hi);
  let a = 0, b = 0, out = lo;
  while (a < left.length && b < right.length) {
    t.metrics.comparisons += 1;
    t.metrics.reads += 2;
    eventGuard(t, "c", lo + a, mid + b, left[a] - right[b], depth);
    t.write(out++, left[a] <= right[b] ? left[a++] : right[b++], depth);
  }
  while (a < left.length) t.write(out++, left[a++], depth);
  while (b < right.length) t.write(out++, right[b++], depth);
}

function eventGuard(t, op, a, b, value, depth) {
  t.metrics.operations += 1;
  if (t.metrics.operations > MAX_OPS) throw new Error("program exceeded operation budget");
  t.metrics.maxDepth = Math.max(t.metrics.maxDepth, depth);
  t.metrics.distance += Math.abs(a - b);
  if (t.events.length < MAX_TRACE) t.events.push([op, a, b, value]);
}

function quick(t, lo = 0, hi = t.values.length, depth = 0) {
  if (hi - lo <= 1) return;
  const pivot = t.read(hi - 1);
  let wall = lo;
  for (let i = lo; i < hi - 1; i += 1) {
    if (t.compareValue(i, pivot, depth) <= 0) t.swap(i, wall++, depth);
  }
  t.swap(wall, hi - 1, depth);
  quick(t, lo, wall, depth + 1);
  quick(t, wall + 1, hi, depth + 1);
}

function heap(t, lo = 0, hi = t.values.length, depth = 0) {
  const length = hi - lo;
  function sift(root, end) {
    for (;;) {
      const left = root * 2 + 1;
      if (left >= end) return;
      let child = left;
      if (left + 1 < end && t.compare(lo + left, lo + left + 1, depth) < 0) child = left + 1;
      if (t.compare(lo + root, lo + child, depth) >= 0) return;
      t.swap(lo + root, lo + child, depth);
      root = child;
    }
  }
  for (let root = Math.floor(length / 2) - 1; root >= 0; root -= 1) sift(root, length);
  for (let end = length - 1; end > 0; end -= 1) { t.swap(lo, lo + end, depth); sift(0, end); }
}

const RUNNERS = Object.freeze({ bubble, insertion, selection, merge, quick, heap });

function runPlan(plan, input) {
  const trace = makeTrace(input);
  if (plan.kind === "sort") {
    RUNNERS[plan.algorithm](trace);
  } else {
    const algorithm = input.length <= plan.threshold ? plan.small : plan.large;
    RUNNERS[algorithm](trace);
  }
  return trace;
}

function isSorted(values) {
  for (let i = 1; i < values.length; i += 1) if (values[i - 1] > values[i]) return false;
  return true;
}

function multiset(values) {
  const counts = new Map();
  for (const value of values) counts.set(value, (counts.get(value) || 0) + 1);
  return [...counts].sort(([a], [b]) => a - b);
}

function verificationInputs(seed = "sort-soup-verification") {
  const rng = new Prng(seed);
  const random = (length, range = 64) => Array.from({ length }, () => Math.floor(rng.float() * range));
  return [
    [], [1], [2, 1], [1, 1, 0],
    Array.from({ length: 24 }, (_, i) => i),
    Array.from({ length: 24 }, (_, i) => 23 - i),
    random(17, 8), random(32), random(63, 256),
  ];
}

function descriptorFrom(runs) {
  const aggregate = runs.reduce((out, run) => {
    for (const key of Object.keys(out)) out[key] += run.metrics[key] || 0;
    out.length += run.values.length;
    return out;
  }, { comparisons: 0, swaps: 0, writes: 0, reads: 0, maxDepth: 0, distance: 0, operations: 0, length: 0 });
  const n = Math.max(1, aggregate.length);
  const comparisons = clamp(Math.log2(1 + aggregate.comparisons / n) / 5);
  const writes = clamp(Math.log2(1 + aggregate.writes / n) / 5);
  const locality = clamp(1 - aggregate.distance / Math.max(1, aggregate.operations * 32));
  const depth = clamp(aggregate.maxDepth / 16);
  const swaps = clamp(Math.log2(1 + aggregate.swaps / n) / 4);
  return [comparisons, writes, locality, depth, swaps].map((value) => round(value));
}

function distance(a, b) {
  return Math.sqrt(a.reduce((sum, value, index) => sum + (value - b[index]) ** 2, 0));
}

function publicCandidate(candidate, { includeGroove = false } = {}) {
  if (!candidate) return null;
  const value = {
    id: candidate.id,
    domain: candidate.domain || "sort",
    source: candidate.source,
    type: candidate.type,
    origin: candidate.origin,
    parent: candidate.parent,
    generation: candidate.generation,
    status: candidate.status,
    aliveness: candidate.aliveness,
    tags: candidate.tags,
    retained: candidate.retained,
    engine: candidate.engine,
    descriptor: candidate.descriptor,
    cell: candidate.cell,
    niche: candidate.niche,
    quality: candidate.quality,
    novelty: candidate.novelty,
    metrics: candidate.metrics,
    iteration: candidate.iteration,
    error: candidate.error,
    entropy: candidate.entropy,
    hardware: candidate.hardware,
    visualReview: candidate.visualReview,
    capabilityHints: candidate.capabilityHints,
    capabilityLineage: candidate.capabilityLineage,
  };
  if (candidate.sample) {
    value.sample = candidate.domain === "raster" ? {
      width: candidate.sample.width, height: candidate.sample.height, rgb: candidate.sample.rgb,
      bytecode: candidate.sample.bytecode, grooveVersion: candidate.sample.grooveVersion,
      grooveBytes: candidate.sample.grooveBytes, grooveHash: candidate.sample.grooveHash,
      groovePc: candidate.sample.groovePc, energy: candidate.sample.energy, frames: candidate.sample.frames,
    } : { input: candidate.sample.input, trace: candidate.sample.trace.slice(0, 512) };
    if (includeGroove && candidate.domain === "raster") value.sample.groove = candidate.sample.groove;
  }
  return value;
}

function storedCandidate(candidate, { includeGroove = false } = {}) {
  if (!candidate) return null;
  return Object.fromEntries(Object.entries({
    id: candidate.id,
    domain: candidate.domain || "sort",
    source: candidate.source,
    origin: candidate.origin,
    parent: candidate.parent,
    generation: candidate.generation,
    status: candidate.status,
    aliveness: candidate.aliveness,
    tags: candidate.tags,
    retained: candidate.retained,
    novelty: candidate.novelty,
    quality: candidate.quality,
    iteration: candidate.iteration,
    error: candidate.error,
    entropy: candidate.entropy,
    hardware: candidate.hardware,
    visualReview: candidate.visualReview,
    capabilityHints: candidate.capabilityHints,
    capabilityLineage: candidate.capabilityLineage,
    grooveVersion: candidate.sample?.grooveVersion,
    groove: includeGroove ? candidate.sample?.groove : undefined,
  }).filter(([, value]) => value !== undefined));
}

function restoreCandidate(stored, migrationProfile = null) {
  if (stored.status === "rejected") return { ...stored };
  const storedRecord = stored.groove ? inspectPixelGroove(stored.groove) : null;
  const legacyProfile = stored.domain === "raster" && !stored.hardware
    ? storedRecord?.valid && storedRecord.hardware ? storedRecord.hardware.name :
      migrationProfile || PROFILE_LIST[Math.abs(Number(stored.iteration) || 0) % PROFILE_LIST.length].name
    : stored.hardware?.name;
  const candidate = (stored.domain === "raster" ? evaluateRasterProgram : evaluateSortProgram)(stored.source, {
    origin: stored.origin,
    parent: stored.parent,
    generation: stored.generation,
    entropySource: stored.entropy?.source,
    authorityUtcMs: stored.entropy?.authorityUtcMs,
    entropySeed: stored.entropy?.seed,
    reviewParent: stored.capabilityLineage?.reviewParent,
    reviewCriticism: stored.capabilityLineage?.criticism,
    inheritedCapability: stored.capabilityLineage?.capability,
    mutationHints: stored.capabilityLineage?.mutationHints,
    profile: legacyProfile,
    _id: stored.id,
  });
  if (stored.id && candidate.id !== stored.id) throw new Error(`stored candidate identity changed: ${stored.id}`);
  if (stored.groove && storedRecord?.valid) {
    const record = storedRecord;
    if (record.id !== candidate.id) throw new Error(`stored PixelGroove is invalid for ${candidate.id}`);
    candidate.sample.groove = stored.groove;
    candidate.sample.grooveVersion = record.version;
    candidate.sample.grooveBytes = GROOVE_LAYOUT.bytes;
    candidate.sample.grooveHash = record.protectedHash;
    candidate.sample.groovePc = record.pc;
  } else if (stored.groove && stored.hardware) {
    throw new Error(`stored PixelGroove is invalid for ${candidate.id}`);
  }
  return { ...candidate, ...stored, sample: candidate.sample };
}

function displayInput(seed) {
  const rng = new Prng(seed);
  return Array.from({ length: 32 }, () => Math.floor(rng.float() * 96));
}

export function evaluateSortProgram(source, { origin = "classic", parent = null, generation = 0 } = {}) {
  const plan = compileSortLisp(source);
  const runs = verificationInputs().map((input) => runPlan(plan, input));
  for (let i = 0; i < runs.length; i += 1) {
    if (!isSorted(runs[i].values)) throw new Error(`verification failed: output ${i} is not sorted`);
    if (JSON.stringify(multiset(runs[i].values)) !== JSON.stringify(multiset(verificationInputs()[i]))) {
      throw new Error(`verification failed: output ${i} is not a permutation`);
    }
  }
  const descriptor = descriptorFrom(runs);
  const id = hash(`${plan.source}\n${parent || "root"}\n${generation}`).slice(0, 12);
  const sampleInput = displayInput(id);
  const sample = runPlan(plan, sampleInput);
  const averageOps = runs.reduce((sum, run) => sum + run.metrics.operations, 0) / runs.length;
  return {
    id,
    domain: "sort",
    source: plan.source,
    type: "Vector<Int,n> -> SortedPermutation<Int,n>",
    plan,
    origin,
    parent,
    generation,
    status: "verified",
    engine: "reference-js",
    descriptor,
    cell: [Math.min(GRID.width - 1, Math.floor(descriptor[0] * GRID.width)), Math.min(GRID.height - 1, Math.floor(descriptor[2] * GRID.height))],
    quality: round(1 / (1 + averageOps / 500)),
    metrics: runs.reduce((out, run) => {
      out.comparisons += run.metrics.comparisons;
      out.swaps += run.metrics.swaps;
      out.writes += run.metrics.writes;
      out.operations += run.metrics.operations;
      out.maxDepth = Math.max(out.maxDepth, run.metrics.maxDepth);
      return out;
    }, { comparisons: 0, swaps: 0, writes: 0, operations: 0, maxDepth: 0 }),
    sample: { input: sampleInput, output: sample.values, trace: sample.events },
  };
}

export class NoveltyArchive {
  constructor({ seed = "piecefarm-sort-soup-v1", maxRecent = 96 } = {}) {
    this.seed = seed;
    this.rng = new Prng(seed);
    this.maxRecent = maxRecent;
    this.iteration = 0;
    this.cells = new Map();
    this.recent = [];
    this.reviewAdvice = [];
    this.rejected = 0;
    this.accepted = 0;
  }

  static fromJSON(value) {
    if (!value || ![1, 2, 3].includes(value.schema) || typeof value.seed !== "string") throw new TypeError("invalid sort-soup archive");
    const archive = new NoveltyArchive({ seed: value.seed, maxRecent: value.maxRecent });
    archive.rng = Prng.fromJSON(value.rng);
    archive.iteration = Number(value.iteration) || 0;
    archive.rejected = Number(value.rejected) || 0;
    archive.accepted = Number(value.accepted) || 0;
    archive.cells = new Map((value.cells || []).map(([, candidate], index) => {
      const restored = restoreCandidate(candidate, PROFILE_LIST[index % PROFILE_LIST.length].name);
      return [`${restored.domain || "sort"}:${(restored.niche || restored.cell).join(":")}`, restored];
    }));
    archive.recent = (value.recent || []).map((candidate, index) =>
      restoreCandidate(candidate, PROFILE_LIST[index % PROFILE_LIST.length].name)).slice(-archive.maxRecent);
    archive.reviewAdvice = (value.reviewAdvice || []).filter((advice) => advice?.specimenId && Array.isArray(advice.mutationHints)).slice(-32);
    archive.pieceVm = value.pieceVm || null;
    return archive;
  }

  toJSON() {
    return {
      schema: 3,
      seed: this.seed,
      maxRecent: this.maxRecent,
      rng: this.rng.toJSON(),
      iteration: this.iteration,
      accepted: this.accepted,
      rejected: this.rejected,
      cells: [...this.cells.entries()].map(([key, candidate]) => [key, storedCandidate(candidate, { includeGroove: candidate.domain === "raster" })]),
      recent: this.recent.map(storedCandidate),
      reviewAdvice: this.reviewAdvice,
      pieceVm: this.pieceVm || null,
    };
  }

  novelty(descriptor, domain = "sort") {
    const distances = [...this.cells.values()].filter((candidate) => (candidate.domain || "sort") === domain)
      .map((candidate) => distance(descriptor, candidate.descriptor)).sort((a, b) => a - b);
    if (!distances.length) return 1;
    const nearest = distances.slice(0, Math.min(5, distances.length));
    return round(nearest.reduce((sum, value) => sum + value, 0) / nearest.length);
  }

  submit(source, options = {}) {
    let candidate = null;
    let error = null;
    try {
      candidate = String(source).trimStart().startsWith("(raster") ? evaluateRasterProgram(source, options) : evaluateSortProgram(source, options);
    } catch (failure) {
      error = failure;
    }
    return this.submitEvaluation({ source, options, candidate, error });
  }

  submitEvaluation({ source, options = {}, candidate = null, error = null }) {
    this.iteration += 1;
    if (candidate && !error) {
      candidate = { ...candidate };
      candidate.novelty = this.novelty(candidate.descriptor, candidate.domain);
      candidate.iteration = this.iteration;
      const key = `${candidate.domain}:${(candidate.niche || candidate.cell).join(":")}`;
      const incumbent = this.cells.get(key);
      candidate.retained = !incumbent || candidate.quality > incumbent.quality || candidate.novelty > incumbent.novelty * 1.15;
      candidate.status = candidate.retained ? "resident" : "dissolving";
      if (candidate.retained) { this.cells.set(key, candidate); this.accepted += 1; }
      else this.rejected += 1;
    } else {
      candidate = {
        id: hash(`${source}\n${this.iteration}`).slice(0, 12), source: String(source).slice(0, MAX_SOURCE),
        origin: options.origin || "unknown", parent: options.parent || null, generation: options.generation || 0,
        iteration: this.iteration, status: "rejected", retained: false, error: String(error?.message || error || "evaluation failed"),
      };
      this.rejected += 1;
    }
    this.recent.push(candidate);
    if (this.recent.length > this.maxRecent) this.recent.splice(0, this.recent.length - this.maxRecent);
    return candidate;
  }

  recordVisualReview(id, review) {
    const matches = new Set([...this.cells.values(), ...this.recent].filter((candidate) => candidate.id === id));
    for (const candidate of matches) {
      candidate.visualReview = { ...review };
      candidate.capabilityHints = [...new Set(review?.mutationHints || [])].slice(0, 3);
    }
    const advice = {
      specimenId: id, trigger: String(review?.trigger || "visual-novelty"),
      at: String(review?.at || ""), criticism: String(review?.criticism || "").slice(0, 180),
      capability: String(review?.capability || "none"),
      mutationHints: [...new Set(review?.mutationHints || [])].slice(0, 3),
    };
    if (advice.mutationHints.length) {
      this.reviewAdvice = this.reviewAdvice.filter((entry) => entry.specimenId !== id || entry.trigger !== advice.trigger);
      this.reviewAdvice.push(advice);
      if (this.reviewAdvice.length > 32) this.reviewAdvice.splice(0, this.reviewAdvice.length - 32);
    }
    return matches.size > 0;
  }

  seedClassics() {
    for (const algorithm of ALGORITHMS) this.submit(`(sort ${algorithm})`, { origin: "classic", generation: 0 });
    for (const threshold of [4, 8, 16, 32]) {
      this.submit(`(hybrid ${threshold} insertion merge)`, { origin: "classic", generation: 0 });
      this.submit(`(hybrid ${threshold} insertion quick)`, { origin: "classic", generation: 0 });
    }
    [
      "(raster (shift 1 0))", "(raster (mix 1 0 96))", "(raster (blur))",
      "(raster (edges))", "(raster (solarize 128))", "(raster (channels gbr))",
      "(raster (xor 64 128 255))", "(raster (rotate) (mix 1 1 144))",
      "(raster (line 8 8 119 96 255 80 32))",
      "(raster (triangle 12 110 64 8 116 110 32 224 255))",
      "(raster (flood 64 64 36 255 40 160))", "(raster (and 240 192 128) (or 4 16 32))",
      "(raster (box 8 8 112 112 32 0) (box 32 32 64 64 96 3))",
      "(raster (copy 16 16 32 32 0) (shift 3 2) (paste 0 80 72 xor))",
      "(raster (cellular 8 12) (mix 1 0 72))",
    ].forEach((source, index) => this.submit(source, {
      origin: "classic", generation: 0, profile: PROFILE_LIST[index % PROFILE_LIST.length].name,
    }));
  }

  seedFoundations() {
    const foundations = [
      "(raster (copy 16 16 32 32 0) (shift 3 2) (paste 0 80 72 xor))",
    ];
    const known = new Set([...this.cells.values(), ...this.recent].map((candidate) => candidate.source));
    return foundations.filter((source) => !known.has(source))
      .map((source) => this.submit(source, { origin: "foundation", generation: 0 }));
  }

  proposeMutation(parentId = null, { authorityUtcMs = 0 } = {}) {
    const memoryWord = this.rng.nextUint();
    const authority = Math.max(0, Math.floor(Number(authorityUtcMs) || 0));
    const rng = new Prng(`${this.seed}:${this.iteration}:${memoryWord}:${authority}`);
    const residents = [...this.cells.values()].filter((candidate) => candidate.domain === "raster");
    const reviewed = residents.filter((candidate) => candidate.capabilityHints?.length);
    const parent = residents.find((candidate) => candidate.id === parentId) ||
      (reviewed.length && rng.float() < .35 ? reviewed[Math.floor(rng.float() * reviewed.length)] :
        residents.length ? residents[Math.floor(rng.float() * residents.length)] : null);
    const signed = () => Math.floor(rng.float() * 9) - 4;
    const byte = () => Math.floor(rng.float() * 256);
    const point = () => `${Math.floor(rng.float() * RASTER.width)} ${Math.floor(rng.float() * RASTER.height)}`;
    const color = () => `${byte()} ${byte()} ${byte()}`;
    const initializedSprites = new Set();
    const copyStage = () => {
      const x = Math.floor(rng.float() * 97), y = Math.floor(rng.float() * 97);
      const slot = Math.floor(rng.float() * RASTER.spriteSlots);
      initializedSprites.add(slot);
      return `(copy ${x} ${y} ${1 + Math.floor(rng.float() * Math.min(RASTER.spriteSize, RASTER.width - x))} ${1 + Math.floor(rng.float() * Math.min(RASTER.spriteSize, RASTER.height - y))} ${slot})`;
    };
    const stage = () => {
      const pick = Math.floor(rng.float() * 20);
      if (pick === 0) return `(add ${signed() * 16} ${signed() * 16} ${signed() * 16})`;
      if (pick === 1) return `(xor ${byte()} ${byte()} ${byte()})`;
      if (pick === 2) return `(and ${color()})`;
      if (pick === 3) return `(or ${color()})`;
      if (pick === 4) return `(shift ${signed()} ${signed()})`;
      if (pick === 5) return `(mix ${signed()} ${signed()} ${byte()})`;
      if (pick === 6) return `(solarize ${byte()})`;
      if (pick === 7) return "(blur)";
      if (pick === 8) return "(edges)";
      if (pick === 9) return "(rotate)";
      if (pick === 10) return `(mirror ${rng.float() < .5 ? "x" : "y"})`;
      if (pick === 11) return `(channels ${["rbg", "grb", "gbr", "brg", "bgr"][Math.floor(rng.float() * 5)]})`;
      if (pick === 12) return `(line ${point()} ${point()} ${color()})`;
      if (pick === 13) return `(triangle ${point()} ${point()} ${point()} ${color()})`;
      if (pick === 14) return `(flood ${point()} ${Math.floor(rng.float() * 96)} ${color()})`;
      if (pick === 15) {
        const x = Math.floor(rng.float() * 96), y = Math.floor(rng.float() * 96);
        const width = 8 + Math.floor(rng.float() * (RASTER.width - x - 7));
        const height = 8 + Math.floor(rng.float() * (RASTER.height - y - 7));
        return `(box ${x} ${y} ${width} ${height} ${byte()} ${Math.floor(rng.float() * 4)})`;
      }
      if (pick === 16) return copyStage();
      if (pick === 17) {
        if (!initializedSprites.size) return copyStage();
        const slots = [...initializedSprites];
        return `(paste ${slots[Math.floor(rng.float() * slots.length)]} ${point()} ${["replace", "xor", "add", "mask"][Math.floor(rng.float() * 4)]})`;
      }
      if (pick === 18) return `(cellular ${[8, 40, 72][Math.floor(rng.float() * 3)]} ${[12, 36, 76][Math.floor(rng.float() * 3)]})`;
      return `(line ${point()} ${point()} ${color()})`;
    };
    const capabilityStage = (hint) => {
      if (hint === "add-feedback" || hint === "stabilize-temporal") return [`(mix ${signed()} ${signed()} ${64 + Math.floor(rng.float() * 128)})`];
      if (hint === "add-masking") return [`(and ${128 + Math.floor(rng.float() * 128)} ${128 + Math.floor(rng.float() * 128)} ${128 + Math.floor(rng.float() * 128)})`];
      if (hint === "add-symmetry") return [`(mirror ${rng.float() < .5 ? "x" : "y"})`];
      if (hint === "add-displacement") return [`(shift ${signed()} ${signed()})`];
      if (hint === "add-cellular") return [`(cellular ${[8, 40, 72][Math.floor(rng.float() * 3)]} ${[12, 36, 76][Math.floor(rng.float() * 3)]})`];
      if (hint === "add-tiling" || hint === "use-sprites") {
        const copied = copyStage(), slots = [...initializedSprites], slot = slots.at(-1);
        return [copied, `(paste ${slot} ${point()} ${["replace", "xor", "add", "mask"][Math.floor(rng.float() * 4)]})`];
      }
      if (hint === "deepen-boxes") {
        const inset = 8 + Math.floor(rng.float() * 24), extent = RASTER.width - inset * 2;
        return [`(box ${inset} ${inset} ${extent} ${extent} ${32 + Math.floor(rng.float() * 160)} ${Math.floor(rng.float() * 4)})`];
      }
      if (hint === "diversify-color") return [`(channels ${["rbg", "grb", "gbr", "brg", "bgr"][Math.floor(rng.float() * 5)]})`];
      if (hint === "increase-contrast") return [`(solarize ${80 + Math.floor(rng.float() * 112)})`];
      if (hint === "reduce-noise") return ["(blur)"];
      return [];
    };
    const parentAdvice = parent?.capabilityHints?.length ? {
      specimenId: parent.id, criticism: parent.visualReview?.criticism || "",
      mutationHints: parent.capabilityHints,
    } : null;
    const inheritedAdvice = parentAdvice || (this.reviewAdvice.length && rng.float() < .2
      ? this.reviewAdvice[Math.floor(rng.float() * this.reviewAdvice.length)] : null);
    const inheritedHints = inheritedAdvice?.mutationHints || [];
    const inherited = inheritedHints.length && rng.float() < .75
      ? inheritedHints[Math.floor(rng.float() * inheritedHints.length)] : null;
    const capabilityByHint = {
      "add-feedback": "feedback", "stabilize-temporal": "feedback",
      "add-masking": "masking", "add-symmetry": "symmetry", "add-tiling": "tiling",
      "add-displacement": "displacement", "add-cellular": "cellular",
      "use-sprites": "sprite-memory", "deepen-boxes": "boxed-computation",
      "diversify-color": "color-dynamics", "increase-contrast": "color-dynamics",
      "reduce-noise": "smoothing",
    };
    const recipe = inherited ? capabilityStage(inherited) : [];
    const stages = 1 + Math.floor(rng.float() * 6);
    const source = `(raster ${[...recipe, ...Array.from({ length: stages }, stage)].slice(0, RASTER.maxStages).join(" ")})`;
    const profile = parent?.hardware && rng.float() < .7
      ? parent.hardware.name
      : PROFILE_LIST[Math.floor(rng.float() * PROFILE_LIST.length)].name;
    return { source, options: {
      origin: "grammar",
      parent: parent?.id || null,
      generation: (parent?.generation || 0) + 1,
      entropySource: authority ? "ac-utc+memory" : "memory",
      authorityUtcMs: authority,
      entropySeed: memoryWord,
      profile,
      reviewParent: inherited ? inheritedAdvice.specimenId : null,
      reviewCriticism: inherited ? inheritedAdvice.criticism : null,
      inheritedCapability: inherited ? capabilityByHint[inherited] : null,
      mutationHints: inherited ? [inherited] : [],
    } };
  }

  mutate(parentId = null) {
    const proposal = this.proposeMutation(parentId);
    return this.submit(proposal.source, proposal.options);
  }

  fork(id) {
    const parentEntry = [...this.cells.entries()].find(([, candidate]) => candidate.id === id);
    if (!parentEntry || parentEntry[1].domain !== "raster") return null;
    const [parentKey, parent] = parentEntry;
    const child = this.mutate(id);
    if (![...this.cells.values()].some((candidate) => candidate.id === id)) {
      for (const [key, candidate] of this.cells) if (candidate.id === child.id) this.cells.delete(key);
      this.cells.set(parentKey, parent);
      child.retained = false;
      child.status = "dissolving";
    }
    child.forkedFrom = id;
    return child;
  }

  retire(id, reason = "low-performer") {
    let retired = null;
    for (const [key, candidate] of this.cells) {
      if (candidate.id !== id) continue;
      this.cells.delete(key);
      candidate.retained = false;
      candidate.status = "retired";
      candidate.retiredReason = reason;
      retired = candidate;
      break;
    }
    for (const candidate of this.recent) if (candidate.id === id) {
      candidate.retained = false;
      candidate.status = "retired";
      candidate.retiredReason = reason;
    }
    return retired;
  }

  snapshot({ selected = null, includeGrooves = false } = {}) {
    const residents = [...this.cells.values()].sort((a, b) => a.cell[1] - b.cell[1] || a.cell[0] - b.cell[0]);
    const recent = this.recent.filter((candidate) => candidate.domain === "raster").slice(-60);
    const rasterResidents = residents.filter((candidate) => candidate.domain === "raster");
    const sortResidents = residents.filter((candidate) => (candidate.domain || "sort") === "sort");
    const visibleById = new Map([...rasterResidents, ...recent].map((candidate) => [candidate.id, candidate]));
    const active = visibleById.get(selected) || recent.at(-1) || residents[0] || null;
    return {
      schema: 2,
      name: "piecefarm-field-soup",
      score: "grow verified programs directly in bounded RGB memory",
      signature: "PixelGroove<160,RGB24,v1> -> RGBField<Q|H|1X|2X,u8>",
      iteration: this.iteration,
      accepted: this.accepted,
      rejected: this.rejected,
      coverage: this.cells.size,
      capacity: GRID.width * GRID.height + RASTER_CAPACITY,
      domains: {
        sort: { coverage: sortResidents.length, capacity: GRID.width * GRID.height },
        raster: { coverage: rasterResidents.length, capacity: RASTER_CAPACITY },
      },
      memory: {
        fieldBytes: (active?.hardware?.fieldBytes || BASE_RASTER.bytes) + BASE_RASTER.permaBytes,
        workingBytes: active?.hardware?.workingBytes || BASE_RASTER.bytes * 2 + BASE_RASTER.permaBytes,
        permaMargin: RASTER.permaMargin, permaCellBytes: RASTER.permaCellBytes,
        grooveVersion: GROOVE_LAYOUT.version, grooveBytes: GROOVE_LAYOUT.bytes,
        marginCoreCells: RASTER.marginCoreCells, marginProtectedCells: RASTER.marginProtectedCells,
        marginFringeCells: RASTER.permaCells - RASTER.marginProtectedCells, spriteSlots: RASTER.spriteSlots,
        residentBytes: rasterResidents.reduce((sum, candidate) => sum + (candidate.hardware?.fieldBytes || BASE_RASTER.bytes) + BASE_RASTER.permaBytes, 0),
        profiles: Object.fromEntries(PROFILE_LIST.map((profile) => [profile.name, {
          resolution: profile.resolution,
          residents: rasterResidents.filter((candidate) => candidate.hardware?.name === profile.name).length,
        }])),
        activeReads: active?.domain === "raster" ? active.metrics?.reads || 0 : 0,
        activeWrites: active?.domain === "raster" ? active.metrics?.writes || 0 : 0,
      },
      aliveness: rasterResidents.reduce((counts, candidate) => {
        counts[candidate.aliveness || "unknown"] = (counts[candidate.aliveness || "unknown"] || 0) + 1; return counts;
      }, { alive: 0, dormant: 0, collapsed: 0, flicker: 0, unknown: 0 }),
      grid: GRID,
      capabilityBakes: {
        advice: this.reviewAdvice.length,
        residents: rasterResidents.filter((candidate) => candidate.capabilityLineage).length,
      },
      pieceVm: this.pieceVm ? {
        schema: this.pieceVm.schema, iteration: this.pieceVm.iteration,
        accepted: this.pieceVm.accepted, rejected: this.pieceVm.rejected,
        championId: this.pieceVm.championId,
      } : null,
      selected: active?.id || null,
      active: publicCandidate(active, { includeGroove: includeGrooves }),
      programs: [...visibleById.values()].map((candidate) => publicCandidate(candidate, { includeGroove: includeGrooves })),
      rng: this.rng.toJSON(),
    };
  }
}

export { ALGORITHMS, GRID, RASTER, RASTER_CAPACITY };
