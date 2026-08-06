import { SonicDeduper, spatialize } from "./spatial-audio.mjs";

const canvas = document.querySelector("#terrarium");
const status = document.querySelector("#status");
const connectButton = document.querySelector("#connect");
const prodButton = document.querySelector("#prod");
const capabilityInput = document.querySelector("#capability");
const organInput = document.querySelector("#organ");
const modalityInput = document.querySelector("#modality");
const stimulusInput = document.querySelector("#stimulus");
const gl = canvas.getContext("webgl2", { antialias: true, alpha: false });
const camera = { x: 0, y: 2.2, z: 14, yaw: 0 };
const keys = new Set();
const deduper = new SonicDeduper();
let capability = "";
let world = { tick: 0, entities: [], visitors: [] };
let audio = null;
let lastSpatialProd = 0;

function shader(type, source) {
  const value = gl.createShader(type);
  gl.shaderSource(value, source);
  gl.compileShader(value);
  if (!gl.getShaderParameter(value, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(value));
  return value;
}

const program = gl.createProgram();
gl.attachShader(program, shader(gl.VERTEX_SHADER, `#version 300 es
  in vec3 position; in float energy; uniform vec2 view; uniform vec3 camera; uniform float yaw;
  out float glow;
  void main() {
    vec3 p = position - camera;
    float c = cos(yaw), s = sin(yaw);
    vec3 q = vec3(p.x*c - p.z*s, p.y, p.x*s + p.z*c);
    float depth = max(1.0, -q.z);
    gl_Position = vec4(q.x / depth / view.x, q.y / depth / view.y, 1.0 - 2.0 / depth, 1.0);
    gl_PointSize = clamp(50.0 / depth + energy * 10.0, 5.0, 34.0);
    glow = energy;
  }`));
gl.attachShader(program, shader(gl.FRAGMENT_SHADER, `#version 300 es
  precision highp float; in float glow; out vec4 color;
  void main() {
    float d = length(gl_PointCoord - vec2(.5)); if (d > .5) discard;
    float a = smoothstep(.5, .1, d); color = vec4(.25 + glow*.3, .75 + glow*.2, .45 + glow*.35, a);
  }`));
gl.linkProgram(program);
if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(program));
const buffer = gl.createBuffer();
const positionLocation = gl.getAttribLocation(program, "position");
const energyLocation = gl.getAttribLocation(program, "energy");

function draw() {
  canvas.width = Math.max(1, Math.floor(canvas.clientWidth * devicePixelRatio));
  canvas.height = Math.max(1, Math.floor(canvas.clientHeight * devicePixelRatio));
  gl.viewport(0, 0, canvas.width, canvas.height);
  gl.clearColor(0.025, 0.075, 0.05, 1);
  gl.clear(gl.COLOR_BUFFER_BIT);
  gl.useProgram(program);
  const values = new Float32Array(world.entities.flatMap((entity) => [entity.x, entity.y, entity.z, entity.energy]));
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.bufferData(gl.ARRAY_BUFFER, values, gl.DYNAMIC_DRAW);
  gl.enableVertexAttribArray(positionLocation);
  gl.vertexAttribPointer(positionLocation, 3, gl.FLOAT, false, 16, 0);
  gl.enableVertexAttribArray(energyLocation);
  gl.vertexAttribPointer(energyLocation, 1, gl.FLOAT, false, 16, 12);
  gl.uniform2f(gl.getUniformLocation(program, "view"), canvas.height / canvas.width, 1);
  gl.uniform3f(gl.getUniformLocation(program, "camera"), camera.x, camera.y, camera.z);
  gl.uniform1f(gl.getUniformLocation(program, "yaw"), camera.yaw);
  gl.drawArrays(gl.POINTS, 0, world.entities.length);
}

function playSonic(event) {
  if (!deduper.accept(event.id) || !audio) return;
  const rendered = spatialize(event.source, camera, event.radius);
  const oscillator = audio.createOscillator();
  const gain = audio.createGain();
  const panner = audio.createStereoPanner();
  oscillator.frequency.value = event.pitch;
  oscillator.type = event.voice.includes("moss") ? "sine" : "triangle";
  panner.pan.value = rendered.pan;
  gain.gain.setValueAtTime(Math.max(0.0001, rendered.gain * event.intensity * 0.18), audio.currentTime);
  gain.gain.exponentialRampToValueAtTime(0.0001, audio.currentTime + event.duration);
  oscillator.connect(gain).connect(panner).connect(audio.destination);
  oscillator.start();
  oscillator.stop(audio.currentTime + event.duration);
  status.dataset.lastSonicPan = rendered.pan.toFixed(3);
  status.dataset.lastSonicGain = rendered.gain.toFixed(3);
}

async function connect() {
  capability = capabilityInput.value.trim();
  audio ||= new AudioContext();
  await audio.resume();
  const response = await fetch("/api/stream", { headers: { Authorization: `Bearer ${capability}` } });
  if (!response.ok) throw new Error(`mediorgan refused (${response.status})`);
  prodButton.disabled = false;
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
      const message = JSON.parse(line);
      if (message.state) world = message.state;
      if (message.type === "sonic") playSonic(message.event);
      status.textContent = `mediorgan connected as ${message.handle || world.visitors[0]?.handle || "visitor"} • tick ${world.tick} • ${world.entities.length} organisms`;
      status.dataset.connected = "true";
      draw();
    }
  }
}

async function prod() {
  const response = await fetch("/api/prod", {
    method: "POST",
    headers: { Authorization: `Bearer ${capability}`, "Content-Type": "application/json" },
    body: JSON.stringify({ target: organInput.value, modality: modalityInput.value, stimulus: stimulusInput.value, position: camera }),
  });
  if (!response.ok) throw new Error((await response.json()).error);
  const result = await response.json();
  status.dataset.lastProdId = result.prodId;
}

connectButton.addEventListener("click", () => connect().catch((error) => { status.textContent = error.message; }));
prodButton.addEventListener("click", () => prod().catch((error) => { status.textContent = error.message; }));
window.addEventListener("keydown", (event) => keys.add(event.key.toLowerCase()));
window.addEventListener("keyup", (event) => keys.delete(event.key.toLowerCase()));

function move() {
  let forward = (keys.has("w") || keys.has("arrowup") ? 1 : 0) - (keys.has("s") || keys.has("arrowdown") ? 1 : 0);
  let turn = (keys.has("d") || keys.has("arrowright") ? 1 : 0) - (keys.has("a") || keys.has("arrowleft") ? 1 : 0);
  const pad = navigator.getGamepads?.()[0];
  if (pad) {
    forward -= Math.abs(pad.axes[1]) > 0.15 ? pad.axes[1] : 0;
    turn += Math.abs(pad.axes[0]) > 0.15 ? pad.axes[0] : 0;
  }
  camera.yaw += turn * 0.025;
  camera.x -= Math.sin(camera.yaw) * forward * 0.08;
  camera.z -= Math.cos(camera.yaw) * forward * 0.08;
  if (capability && performance.now() - lastSpatialProd > 250 && (Math.abs(forward) + Math.abs(turn) > 0.05)) {
    lastSpatialProd = performance.now();
    fetch("/api/prod", {
      method: "POST",
      headers: { Authorization: `Bearer ${capability}`, "Content-Type": "application/json" },
      body: JSON.stringify({ target: "spatial", modality: "gesture", stimulus: { position: camera }, position: camera }),
    }).catch(() => {});
  }
  draw();
  requestAnimationFrame(move);
}
requestAnimationFrame(move);
