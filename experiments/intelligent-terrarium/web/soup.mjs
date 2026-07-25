const canvas = document.getElementById("soup");
const description = document.getElementById("description");
const soupIteration = document.getElementById("soup-iteration");
const soupProgram = document.getElementById("soup-program");
const soupCoverage = document.getElementById("soup-coverage");
const soupVerification = document.getElementById("soup-verification");
const ctx = canvas.getContext("2d", { alpha: false });
let state = { programs: [], selected: null, iteration: 0, coverage: 0, capacity: 96 };
let selectedIndex = 0;
let columns = 4;
let lastMove = 0;
const rasterCache = new Map();
const grooveCache = new Map();
const groovePending = new Set();
const GROOVE_TRACKS = Object.freeze([
  ["header", "HEADER", "#ff5a67"],
  ["sequence", "SEQUENCE", "#ff984f"],
  ["functions", "FUNCTIONS", "#ffd84a"],
  ["bodies", "BODIES", "#93e85f"],
  ["projection", "PROJECTION", "#40dfaa"],
  ["lifecycle", "LIFECYCLE", "#38d9e6"],
  ["state", "STATE", "#4f9cff"],
  ["sprites", "SPRITES", "#8f7cff"],
  ["proposals", "PROPOSALS", "#d96cff"],
  ["source", "SOURCE", "#ff62bc"],
  ["fringe", "FRINGE", "#f7f0d0"],
]);

function grooveCoordinates() {
  const coordinates = [];
  for (let ring = 0; ring < 16; ring += 1) {
    const low = ring, high = 159 - ring;
    for (let x = low; x <= high; x += 1) coordinates.push([x, low]);
    for (let y = low + 1; y <= high; y += 1) coordinates.push([high, y]);
    for (let x = high - 1; x >= low; x -= 1) coordinates.push([x, high]);
    for (let y = high - 1; y > low; y -= 1) coordinates.push([low, y]);
  }
  return coordinates;
}

const groovePixels = grooveCoordinates();

function renderSubstrate(program, groove) {
  const canvas = document.createElement("canvas");
  canvas.width = 160; canvas.height = 160;
  const pixels = new Uint8ClampedArray(160 * 160 * 4);
  const margin = groove.groove;
  for (let pixel = 0; pixel < groovePixels.length; pixel += 1) {
    const [x, y] = groovePixels[pixel], target = (y * 160 + x) * 4, source = pixel * 6;
    pixels[target] = parseInt(margin.slice(source, source + 2), 16);
    pixels[target + 1] = parseInt(margin.slice(source + 2, source + 4), 16);
    pixels[target + 2] = parseInt(margin.slice(source + 4, source + 6), 16);
    pixels[target + 3] = 255;
  }
  const sample = program.sample, side = Number(sample?.width || 0);
  if (sample?.rgb && side > 0 && side === Number(sample.height)) {
    for (let y = 0; y < 128; y += 1) for (let x = 0; x < 128; x += 1) {
      const sx = Math.min(side - 1, Math.floor(x * side / 128));
      const sy = Math.min(side - 1, Math.floor(y * side / 128));
      const source = (sy * side + sx) * 6, target = ((y + 16) * 160 + x + 16) * 4;
      pixels[target] = parseInt(sample.rgb.slice(source, source + 2), 16);
      pixels[target + 1] = parseInt(sample.rgb.slice(source + 2, source + 4), 16);
      pixels[target + 2] = parseInt(sample.rgb.slice(source + 4, source + 6), 16);
      pixels[target + 3] = 255;
    }
  }
  canvas.getContext("2d").putImageData(new ImageData(pixels, 160, 160), 0, 0);
  return canvas;
}

function ensureGroove(program) {
  if (!program?.id || groovePending.has(program.id)) return;
  const cached = grooveCache.get(program.id);
  if (cached && Date.now() - cached.at < 12_000) return;
  groovePending.add(program.id);
  fetch(`/api/groove/${encodeURIComponent(program.id)}`)
    .then((response) => response.ok ? response.json() : Promise.reject(new Error(`groove ${response.status}`)))
    .then((groove) => grooveCache.set(program.id, {
      at: Date.now(), canvas: renderSubstrate(program, groove), needlePixel: groove.record?.needlePixel || 0,
      record: groove.record || null,
    }))
    .catch(() => {})
    .finally(() => groovePending.delete(program.id));
}

const palette = {
  resident: "#94f0bd",
  dissolving: "#a57970",
  rejected: "#f05f78",
  classic: "#ffda68",
  grammar: "#75cfde",
  llm: "#ef82dc",
  prox: "#ef82dc",
};

function fit() {
  const ratio = devicePixelRatio || 1;
  canvas.width = Math.max(1, Math.floor(canvas.clientWidth * ratio));
  canvas.height = Math.max(1, Math.floor(canvas.clientHeight * ratio));
  ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
  ctx.imageSmoothingEnabled = false;
}

function programValues(program, phase) {
  const values = [...(program.sample?.input || [])];
  const trace = program.sample?.trace || [];
  if (!trace.length) return { values, active: null };
  const end = Math.floor(phase * trace.length) % (trace.length + 28);
  let active = null;
  for (let i = 0; i < Math.min(end, trace.length); i += 1) {
    const event = trace[i];
    if (event[0] === "s") [values[event[1]], values[event[2]]] = [values[event[2]], values[event[1]]];
    if (event[0] === "w") values[event[1]] = event[3];
    active = event;
  }
  return { values, active };
}

function tileLayout(count) {
  const w = canvas.clientWidth, h = canvas.clientHeight;
  columns = 4;
  return { columns: 4, rows: 3, width: w / 4, height: h / 3 };
}

function boardPrograms() {
  const addressed = state.displayPrograms?.filter((program) => program?.domain === "raster") || [];
  const programs = addressed.length ? addressed : (state.programs || []).filter((program) => program.domain === "raster").slice(-12);
  return programs.slice(0, 12);
}

function fillField(programs) {
  const layout = tileLayout(programs.length);
  const slots = 12;
  const field = Array.from({ length: slots }, (_, index) => ({
    ...programs[index % programs.length],
    visualEcho: index >= programs.length,
    visualSlot: index,
  }));
  return { field, layout };
}

function drawMembranes(programs, layout) {
  const originals = programs.filter((program) => !program.visualEcho);
  const positions = new Map(originals.map((program) => [program.id, {
    x: (program.visualSlot % layout.columns + .5) * layout.width,
    y: (Math.floor(program.visualSlot / layout.columns) + .5) * layout.height,
  }]));
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  ctx.lineWidth = 1.25;
  for (const program of originals) {
    const a = positions.get(program.id), b = positions.get(program.parent);
    if (!a || !b) continue;
    ctx.strokeStyle = `${palette[program.origin] || "#677"}3d`;
    ctx.beginPath();
    ctx.moveTo(a.x, a.y);
    const mid = (a.x + b.x) / 2;
    ctx.bezierCurveTo(mid, a.y, mid, b.y, b.x, b.y);
    ctx.stroke();
    ctx.fillStyle = `${palette[program.origin] || "#677"}66`;
    ctx.fillRect(a.x - 1.5, a.y - 1.5, 3, 3);
  }
  ctx.restore();
}

function drawGrooveBands(cached, x, y, width, height, left, top, side) {
  const rightGutter = x + width - (left + side);
  const outside = rightGutter >= 38;
  const railWidth = Math.max(4, Math.min(10, side * .025));
  const railX = outside ? left + side + 4 : left + side - railWidth - 3;
  const rowHeight = side / GROOVE_TRACKS.length;
  const density = cached?.record?.density?.tracks || {};
  ctx.save();
  ctx.font = `${Math.max(6, Math.min(8, rowHeight * .3))}px "FiraCode Nerd Font Mono", "Courier New", monospace`;
  ctx.textBaseline = "middle";
  for (let index = 0; index < GROOVE_TRACKS.length; index += 1) {
    const [key, label, color] = GROOVE_TRACKS[index];
    const fill = Math.max(0, Math.min(1, Number(density[key]?.pixelFill) || 0));
    const rowY = top + index * rowHeight;
    ctx.globalAlpha = outside ? .34 : .68;
    ctx.fillStyle = color;
    ctx.fillRect(railX, rowY, railWidth, Math.max(1, rowHeight - 1));
    ctx.globalAlpha = 1;
    ctx.fillRect(railX, rowY, railWidth * fill, Math.max(1, rowHeight - 1));
    if (outside) {
      ctx.fillStyle = color;
      ctx.globalAlpha = .82;
      ctx.fillText(label, railX + railWidth + 4, rowY + rowHeight / 2);
    }
  }
  ctx.restore();
}

function drawRasterBed(program, x, y, width, height, time) {
  ensureGroove(program);
  const cached = grooveCache.get(program.id), substrate = cached?.canvas;
  if (substrate) {
    const side = Math.min(width, height), left = x + (width - side) / 2, top = y + (height - side) / 2;
    ctx.save(); ctx.globalAlpha = program.visualEcho ? .28 : .68;
    ctx.imageSmoothingEnabled = false;
    ctx.drawImage(substrate, left, top, side, side);
    ctx.restore();
    drawGrooveBands(cached, x, y, width, height, left, top, side);
    const live = state.runtime?.vm?.telemetry?.residents?.find((row) => row.id === program.id);
    const needlePixel = Math.max(0, Math.min(groovePixels.length - 1, Number(live?.needlePixel ?? cached.needlePixel) || 0));
    const [needleX, needleY] = groovePixels[needlePixel];
    const scale = side / 160, pulse = .45 + .55 * Math.abs(Math.sin(time * Math.PI / 260));
    ctx.save();
    ctx.globalAlpha = pulse;
    ctx.fillStyle = "#fff7c5";
    ctx.shadowColor = "#ff2ca8"; ctx.shadowBlur = Math.max(5, scale * 4);
    ctx.fillRect(left + needleX * scale, top + needleY * scale, Math.max(2, scale), Math.max(2, scale));
    ctx.restore();
    return;
  }
  const sample = program.sample;
  if (sample?.rgb && sample.width > 0 && sample.height > 0) {
    const key = `${program.id}:${sample.width}:${sample.height}:${sample.rgb.length}`;
    let raster = rasterCache.get(key);
    if (!raster) {
      raster = document.createElement("canvas");
      raster.width = sample.width; raster.height = sample.height;
      const pixels = new Uint8ClampedArray(sample.width * sample.height * 4);
      for (let source = 0, target = 0; source < sample.rgb.length; source += 6, target += 4) {
        pixels[target] = parseInt(sample.rgb.slice(source, source + 2), 16);
        pixels[target + 1] = parseInt(sample.rgb.slice(source + 2, source + 4), 16);
        pixels[target + 2] = parseInt(sample.rgb.slice(source + 4, source + 6), 16);
        pixels[target + 3] = 255;
      }
      raster.getContext("2d").putImageData(new ImageData(pixels, sample.width, sample.height), 0, 0);
      rasterCache.set(key, raster);
    }
    ctx.save(); ctx.globalAlpha = program.visualEcho ? .25 : .48;
    ctx.imageSmoothingEnabled = false;
    ctx.drawImage(raster, x, y, width, height);
    ctx.restore();
    return;
  }
  const seed = [...String(program.id || "0")].reduce((sum, char) => (sum * 33 + char.charCodeAt(0)) >>> 0, 5381);
  const descriptor = program.descriptor || [];
  const rasterColumns = 16;
  const rasterRows = 8;
  const cellWidth = width / rasterColumns;
  const cellHeight = height / rasterRows;
  ctx.save();
  ctx.globalAlpha = program.visualEcho ? .19 : .28;
  for (let row = 0; row < rasterRows; row += 1) {
    for (let column = 0; column < rasterColumns; column += 1) {
      const signal = (seed + column * 37 + row * 71 + Math.floor((descriptor[(column + row) % Math.max(1, descriptor.length)] || 0) * 997)) % 360;
      ctx.fillStyle = `hsl(${signal} 82% ${18 + (signal % 29)}%)`;
      ctx.fillRect(x + column * cellWidth, y + row * cellHeight, Math.ceil(cellWidth), Math.ceil(cellHeight));
    }
  }
  ctx.restore();
}

function drawTile(program, index, layout, time) {
  const col = index % layout.columns, row = Math.floor(index / layout.columns);
  const x = col * layout.width, y = row * layout.height;
  const pad = Math.max(3, Math.min(8, layout.width * .025));
  const selected = program.id === state.selected && !program.visualEcho;
  const statusColor = palette[program.status] || "#64716b";
  const originColor = palette[program.origin] || "#b5c0b6";
  ctx.fillStyle = selected ? "#172329" : index % 2 ? "#091117" : "#071016";
  ctx.fillRect(x + 1, y + 1, layout.width - 2, layout.height - 2);
  drawRasterBed(program, x + 2, y + 2, layout.width - 4, layout.height - 4, time);
  ctx.strokeStyle = selected ? "#fff7c5" : `${statusColor}72`;
  ctx.lineWidth = selected ? 3 : 1;
  ctx.strokeRect(x + 1.5, y + 1.5, layout.width - 3, layout.height - 3);

  const font = Math.max(12, Math.min(22, layout.height * .11));
  ctx.font = `${selected ? "bold " : ""}${font}px "FiraCode Nerd Font Mono", "Courier New", monospace`;
  ctx.textBaseline = "top";
  ctx.fillStyle = originColor;
  ctx.fillText(program.visualEcho ? "↻" : program.status === "rejected" ? "×" : program.retained ? "●" : "·", x + pad, y + pad);
  ctx.fillStyle = "#dce7dc";
  ctx.fillText(program.id || "unread", x + pad + font * 1.2, y + pad);
  const machine = program.hardware || { label: "1X", resolution: 128 };
  const badge = `${machine.label || "1X"} ${machine.resolution || 128}²`;
  ctx.font = `bold ${Math.max(10, font * .72)}px "FiraCode Nerd Font Mono", "Courier New", monospace`;
  const badgeWidth = ctx.measureText(badge).width + pad * 1.5;
  ctx.fillStyle = "#05080ddd"; ctx.fillRect(x + layout.width - badgeWidth - pad, y + pad, badgeWidth, font);
  ctx.fillStyle = "#ffda68"; ctx.fillText(badge, x + layout.width - badgeWidth - pad / 2, y + pad + 1);
  ctx.font = `${selected ? "bold " : ""}${font}px "FiraCode Nerd Font Mono", "Courier New", monospace`;
  ctx.fillStyle = "#b7cfc4";
  const source = program.source || program.error || "(unreadable)";
  ctx.fillText(source.slice(0, Math.max(8, Math.floor((layout.width - pad * 2) / (font * .62)))), x + pad, y + pad + font * 1.35);

  if (program.status === "rejected" || !program.sample) {
    ctx.fillStyle = "#cf6679";
    ctx.fillText((program.error || "rejected").slice(0, 38), x + pad, y + layout.height - pad - font);
    return;
  }

  const phase = (time * .00016 + index * .071) % 1;
  const { values, active } = programValues(program, phase);
  const graphTop = y + pad + font * 3;
  const graphHeight = Math.max(8, layout.height - (graphTop - y) - pad - font * 1.2);
  const cellWidth = Math.max(1, (layout.width - pad * 2) / Math.max(1, values.length));
  const max = Math.max(1, ...values);
  values.forEach((value, valueIndex) => {
    const intensity = value / max;
    const hue = 120 + intensity * 240 + (program.descriptor?.[2] || 0) * 90 + index * 7;
    ctx.fillStyle = `hsl(${hue % 360} 92% ${26 + intensity * 58}%)`;
    const barHeight = Math.max(2, intensity * graphHeight);
    ctx.fillRect(x + pad + valueIndex * cellWidth, graphTop + graphHeight - barHeight, Math.max(1, cellWidth - 1), barHeight);
  });
  if (active) {
    ctx.strokeStyle = active[0] === "c" ? "#ffda68" : active[0] === "s" ? "#ef82dc" : "#75cfde";
    ctx.lineWidth = 2;
    for (const valueIndex of [active[1], active[2]]) {
      if (valueIndex < 0) continue;
      ctx.strokeRect(x + pad + valueIndex * cellWidth, graphTop, Math.max(2, cellWidth), graphHeight);
    }
  }
  ctx.font = `${Math.max(10, font * .78)}px "FiraCode Nerd Font Mono", "Courier New", monospace`;
  const health = state.runtime?.vm?.telemetry?.residents?.find((row) => row.id === program.id);
  const analysis = state.runtime?.vm?.telemetry?.analysis?.find((row) => row.id === program.id);
  ctx.fillStyle = statusColor;
  const footer = `${program.visualEcho ? "ECHO  " : ""}N ${Number(program.novelty || 0).toFixed(2)} Q ${Number(program.quality || 0).toFixed(2)} G${program.generation || 0}`;
  ctx.fillText(footer, x + pad, y + layout.height - pad - font * .72);
  if (health) {
    const barX = x + pad, barY = y + layout.height - Math.max(4, pad * .55);
    const barWidth = layout.width - pad * 2, barHeight = Math.max(3, pad * .32);
    ctx.fillStyle = "#000000"; ctx.fillRect(barX, barY, barWidth, barHeight);
    const healthGradient = ctx.createLinearGradient(barX, 0, barX + barWidth, 0);
    healthGradient.addColorStop(0, "#e32636");
    healthGradient.addColorStop(.34, "#ff7a1a");
    healthGradient.addColorStop(.67, "#ffd84a");
    healthGradient.addColorStop(1, "#72e889");
    ctx.fillStyle = healthGradient;
    ctx.fillRect(barX, barY, barWidth * Math.max(0, Math.min(100, health.hp)) / 100, barHeight);
    if (analysis) {
      ctx.fillStyle = "#ffffff";
      ctx.fillRect(barX + barWidth * Math.max(0, Math.min(100, analysis.cutoff)) / 100 - 1, barY - 2, 2, barHeight + 4);
    }
  }
}

function draw(time = 0) {
  const programs = boardPrograms();
  ctx.fillStyle = "#04080d";
  ctx.fillRect(0, 0, canvas.clientWidth, canvas.clientHeight);
  if (!programs.length) {
    ctx.fillStyle = "#9acbb7";
    ctx.font = "24px monospace";
    ctx.fillText("awakening Lisp Soup…", 24, 24);
    requestAnimationFrame(draw);
    return;
  }
  const { field, layout } = fillField(programs);
  field.forEach((program, index) => drawTile(program, index, layout, time));
  drawMembranes(field, layout);
  requestAnimationFrame(draw);
}

async function select(index) {
  const programs = boardPrograms();
  if (!programs.length) return;
  selectedIndex = (index + programs.length) % programs.length;
  const id = programs[selectedIndex].id;
  state.selected = id;
  await fetch("/api/select", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ id }),
  });
}

function navigate(dx, dy) {
  select(selectedIndex + dx + dy * columns).catch(() => {});
}

window.addEventListener("keydown", (event) => {
  if (event.key === "ArrowLeft" || event.key.toLowerCase() === "a") navigate(-1, 0);
  if (event.key === "ArrowRight" || event.key.toLowerCase() === "d") navigate(1, 0);
  if (event.key === "ArrowUp" || event.key.toLowerCase() === "w") navigate(0, -1);
  if (event.key === "ArrowDown" || event.key.toLowerCase() === "s") navigate(0, 1);
});

function pollGamepad(time) {
  const pad = navigator.getGamepads?.()[0];
  if (pad && time - lastMove > 220) {
    const x = pad.axes[0] || 0, y = pad.axes[1] || 0;
    if (Math.abs(x) > .6 || Math.abs(y) > .6) {
      navigate(Math.abs(x) > .6 ? Math.sign(x) : 0, Math.abs(y) > .6 ? Math.sign(y) : 0);
      lastMove = time;
    }
  }
  requestAnimationFrame(pollGamepad);
}

async function stream() {
  const response = await fetch("/api/stream");
  if (!response.ok) throw new Error(`soup stream refused (${response.status})`);
  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let pending = "";
  for (;;) {
    const { done, value } = await reader.read();
    if (done) break;
    pending += decoder.decode(value, { stream: true });
    const lines = pending.split("\n");
    pending = lines.pop();
    for (const line of lines) {
      if (!line) continue;
      state = JSON.parse(line).state;
      const programs = boardPrograms();
      selectedIndex = Math.max(0, programs.findIndex((program) => program.id === state.selected));
      const active = programs[selectedIndex];
      soupIteration.textContent = `ITER ${state.iteration.toLocaleString()}`;
      soupProgram.textContent = active?.source || "(EMPTY SOUP)";
      soupCoverage.textContent = `${state.coverage} / ${state.capacity} NICHES`;
      soupVerification.textContent = `${state.accepted.toLocaleString()} VERIFIED`;
      description.textContent = `${programs.length} organisms in a fixed 4×3 field; ${state.coverage} archive cells; selected ${active?.source || "none"}.`;
    }
  }
}

window.addEventListener("resize", fit);
fit();
requestAnimationFrame(draw);
requestAnimationFrame(pollGamepad);
stream().catch((error) => { description.textContent = error.message; });
