const get = (id) => document.getElementById(id);
const history = [];
const raster = get("board-raster");
const rasterContext = raster.getContext("2d", { alpha: false });
const rasterWidth = 80;
const rasterHeight = 45;
let rasterState = { iteration: 0, coverage: 0, capacity: 96, accepted: 0, active: {} };
let lastRasterFrame = 0;
let checkpointDeadline = 0;
let checkpointIteration = 0;
let checkpointRemaining = 0;

function text(id, value) { get(id).textContent = value ?? "—"; }

function progress(id, value) {
  get(id).style.width = `${Math.max(0, Math.min(1, Number(value) || 0)) * 100}%`;
}

function rasterSignal(x, y, state, epoch) {
  const active = state.active || {};
  const id = String(active.id || "piecefarm");
  const seed = [...id].reduce((sum, char) => (sum * 33 + char.charCodeAt(0)) >>> 0, 5381);
  return (seed + x * 41 + y * 73 + epoch * 17 + state.iteration * 3) >>> 0;
}

function drawRaster(time) {
  requestAnimationFrame(drawRaster);
  if (time - lastRasterFrame < 125) return;
  lastRasterFrame = time;
  if (raster.width !== rasterWidth || raster.height !== rasterHeight) {
    raster.width = rasterWidth;
    raster.height = rasterHeight;
    rasterContext.imageSmoothingEnabled = false;
  }
  const epoch = Math.floor(time / 750);
  const coverage = rasterState.capacity ? rasterState.coverage / rasterState.capacity : 0;
  const novelty = Number(rasterState.active?.novelty || 0);
  for (let y = 0; y < rasterHeight; y += 1) {
    for (let x = 0; x < rasterWidth; x += 1) {
      const signal = rasterSignal(x, y, rasterState, epoch);
      const band = Math.floor(x / 8) + Math.floor(y / 5);
      const hue = (signal % 360 + coverage * 140 + novelty * 90 + band * 19) % 360;
      const pulse = (signal + epoch + x + y) % 7 === 0;
      rasterContext.fillStyle = `hsl(${hue} ${pulse ? 90 : 72}% ${pulse ? 42 : 19 + signal % 15}%)`;
      rasterContext.fillRect(x, y, 1, 1);
    }
  }
}

function renderCountdown() {
  const remainingMs = Math.max(0, checkpointDeadline - Date.now());
  const seconds = Math.ceil(remainingMs / 1000);
  const hours = String(Math.floor(seconds / 3600)).padStart(2, "0");
  const minutes = String(Math.floor(seconds % 3600 / 60)).padStart(2, "0");
  const remainder = String(seconds % 60).padStart(2, "0");
  text("countdown-clock", `${hours}:${minutes}:${remainder}`);
  text("countdown-detail", `${checkpointRemaining.toLocaleString()} ITERATIONS TO EDITION ${checkpointIteration.toLocaleString()}`);
}

function drawChart() {
  const canvas = get("chart");
  const ratio = devicePixelRatio || 1;
  canvas.width = Math.max(1, Math.floor(canvas.clientWidth * ratio));
  canvas.height = Math.max(1, Math.floor(canvas.clientHeight * ratio));
  const ctx = canvas.getContext("2d");
  ctx.scale(ratio, ratio);
  const w = canvas.clientWidth, h = canvas.clientHeight;
  ctx.clearRect(0, 0, w, h);
  ctx.strokeStyle = "#253a35";
  ctx.lineWidth = 1;
  for (let i = 1; i < 4; i += 1) {
    const y = (h / 4) * i;
    ctx.beginPath(); ctx.moveTo(0, y); ctx.lineTo(w, y); ctx.stroke();
  }
  if (history.length < 2) return;
  ctx.strokeStyle = "#ffda68";
  ctx.lineWidth = 3;
  ctx.beginPath();
  history.forEach((point, index) => {
    const x = index / (history.length - 1) * w;
    const y = h - point * h;
    if (index) ctx.lineTo(x, y); else ctx.moveTo(x, y);
  });
  ctx.stroke();
}

function update(state) {
  const active = state.active || {};
  rasterState = state;
  text("mission", (state.score || "search program-output space without surrendering verification").toUpperCase());
  text("ticker", `ITER ${state.iteration}  ·  VERIFIED ${state.accepted}  ·  COVERAGE ${state.coverage}/${state.capacity}  ·  NOW FARMING ${active.source || "(empty soup)"}`);
  checkpointIteration = Number(state.checkpoint?.nextIteration || 0);
  checkpointRemaining = Number(state.checkpoint?.iterationsRemaining || 0);
  checkpointDeadline = Date.now() + Number(state.checkpoint?.estimatedMs || 0);
  renderCountdown();
  const coverageRatio = state.capacity ? state.coverage / state.capacity : 0;
  const yieldRatio = state.iteration ? state.accepted / state.iteration : 0;
  text("phase-value", "2 / 5");
  progress("phase-fill", 2 / 5);
  text("coverage-value", `${state.coverage} / ${state.capacity}`);
  progress("coverage-fill", coverageRatio);
  text("yield-value", `${(yieldRatio * 100).toFixed(1)}%`);
  progress("yield-fill", yieldRatio);
  text("novelty-value", Number(active.novelty || 0).toFixed(3));
  progress("novelty-fill", active.novelty);
  text("iteration", state.iteration);
  text("novelty", Number(active.novelty || 0).toFixed(3));
  text("coverage", `${state.coverage}/${state.capacity}`);
  text("accepted", state.accepted);
  text("rejected", state.rejected);
  text("operations", Number(active.metrics?.operations || 0).toLocaleString());
  history.push(coverageRatio);
  if (history.length > 240) history.shift();
  drawChart();
}

async function stream() {
  const response = await fetch("/api/stream");
  if (!response.ok) throw new Error(`observatory stream refused (${response.status})`);
  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let pending = "";
  for (;;) {
    const { done, value } = await reader.read();
    if (done) break;
    pending += decoder.decode(value, { stream: true });
    const lines = pending.split("\n");
    pending = lines.pop();
    for (const line of lines) if (line) update(JSON.parse(line).state);
  }
}

window.addEventListener("resize", drawChart);
setInterval(renderCountdown, 250);
requestAnimationFrame(drawRaster);
stream().catch((error) => { text("ticker", `STREAM ERROR · ${error.message}`); });
