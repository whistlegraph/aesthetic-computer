// classic-steps, 2026.11.18
// Every answer in No Paint classic is a step of an Aesthetic Computer
// painting. The first proposal of a painting gets it a #code; each Paint and
// each No after that is one more version of it: #code v12.
//
// The game reports its steps over the bridge (toolchain/nopaint/classic-bridge.js):
// "proposed", then "painted" or "refused". This module turns them into the
// same record Note Paint keeps (lib/nopaint-pieces.mjs: a stack of layers,
// each with the line that made it and the pixels it added) and saves it
// through /api/painting-wip. A Paint is a layer holding the pixels that
// changed. A No is a layer with its line and no pixels.
//
// A guest's painting is held by a key kept in this browser. Nothing here asks
// for a name, and the painting's pixels are what the Save button already sends.
//
// Loaded by index.html. window.nopaintSteps shows the state for debugging.

import { createNoPaintPiece, createNoPaintProposalLayer, appendNoPaintLayer, recoverNoPaintPiece }
  from "https://aesthetic.computer/aesthetic.computer/lib/nopaint-pieces.mjs";
import { encodePaintingState, decodePaintingState } from "https://aesthetic.computer/aesthetic.computer/lib/painting-state.mjs";

const API = "https://aesthetic.computer/api/painting-wip";
const STORE = "nopaint:classic:steps";
const SIDE = 256;            // the painting; the game's canvas is taller
const QUIET = 2500;          // ms without a step before a save goes out
const MAX_STATE = 24_000_000; // base64 characters; the service takes 32 MB of state

const channel = new BroadcastChannel("nopaint-classic");
const waiting = new Map();
let asked = 0;
const ask = (op, args = {}, timeout = 8000) => new Promise((resolve, reject) => {
  const id = `steps${++asked}`;
  waiting.set(id, { resolve, reject });
  channel.postMessage({ to: "game", id, op, ...args });
  setTimeout(() => { if (waiting.delete(id)) reject(new Error(`the game did not answer "${op}"`)); }, timeout);
});

const random = (bytes) => btoa(String.fromCharCode(...crypto.getRandomValues(new Uint8Array(bytes))))
  .replaceAll("+", "-").replaceAll("/", "_").replaceAll("=", "");
const remembered = () => { try { return JSON.parse(localStorage.getItem(STORE) || "null"); } catch { return null; } };
const remember = (value) => { try { localStorage.setItem(STORE, JSON.stringify(value)); } catch {} };

// The top square of the game's Painting canvas, and its hash.
async function painting() {
  const image = await ask("pixels", { object: "Painting", data: true });
  const pixels = new Uint8ClampedArray(SIDE * SIDE * 4);
  for (let y = 0; y < SIDE; y++) pixels.set(image.data.subarray(y * image.width * 4, y * image.width * 4 + SIDE * 4), y * SIDE * 4);
  let hash = 0x811c9dc5;
  for (let i = 0; i < pixels.length; i++) hash = Math.imul(hash ^ pixels[i], 0x01000193);
  return { pixels, hash: (hash >>> 0).toString(16).padStart(8, "0") };
}

async function call(body) {
  const response = await fetch(API, { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) });
  const value = await response.json().catch(() => ({}));
  if (!response.ok) throw Object.assign(new Error(value.error || `painting service answered ${response.status}`), { status: response.status });
  return value;
}

// ---- one painting -----------------------------------------------------------
// session: what this browser remembers. piece: the layers, in memory.
let session = null, piece = null, saving = null, timer = null, dirty = false, failed = null;
const state = (window.nopaintSteps = { get code() { return session?.code || null; }, get version() { return piece ? piece.layers.length - 1 : 0; },
  get saved() { return session?.saved ?? 0; }, get error() { return failed; }, events: [] });
const show = () => {
  chip.hidden = !session?.code;
  chip.textContent = session?.code ? `#${session.code} v${state.version}` : "";
  chip.dataset.saved = String(state.saved === state.version);
};

// Take up the painting on the canvas: the one this browser was making, if the
// canvas still shows its last step, or a new one that starts from the canvas.
async function begin() {
  const now = await painting();
  const kept = remembered();
  if (kept?.code && kept.hash === now.hash) {
    try {
      const read = await call({ action: "read", code: kept.code, key: kept.key, state: true });
      const recovered = read.state ? recoverNoPaintPiece(await decodePaintingState(read.state), SIDE, SIDE) : null;
      if (read.canEdit && recovered) { session = { ...kept, revision: read.revision, saved: recovered.layers.length - 1 }; piece = recovered; return show(); }
    } catch {}
  }
  session = { id: random(24), key: random(32), code: null, revision: 0, hash: now.hash, saved: 0 };
  piece = createNoPaintPiece({ seed: session.id, width: SIDE, height: SIDE, pixels: now.pixels, role: "nopaint-classic" });
  show();
}

// One step: the brush, the options it ran with, and how it was answered.
async function step(event, decision) {
  if (!piece) await begin();
  const score = { kind: event.tool, engine: "nopaint-classic", options: event.options || "", decision,
    line: event.options ? `${event.tool}: ${event.options}` : event.tool };
  let added = new Uint8ClampedArray(SIDE * SIDE * 4), composite = piece.composite.pixels;
  if (decision === "paint") {
    // Keep only what changed. The painting is opaque, so a changed pixel laid
    // over the last picture gives the new one exactly.
    const now = await painting();
    const before = piece.composite.pixels;
    for (let i = 0; i < now.pixels.length; i += 4) {
      if (now.pixels[i] === before[i] && now.pixels[i + 1] === before[i + 1] && now.pixels[i + 2] === before[i + 2] && now.pixels[i + 3] === before[i + 3]) continue;
      added[i] = now.pixels[i]; added[i + 1] = now.pixels[i + 1]; added[i + 2] = now.pixels[i + 2]; added[i + 3] = now.pixels[i + 3] || 255;
    }
    composite = now.pixels;
    session.hash = now.hash;
  }
  const layer = createNoPaintProposalLayer({ piece, proposal: score, proposalNumber: piece.layers.length, proposalFrame: 0, pixels: added });
  piece = appendNoPaintLayer(piece, { ...layer, timestamp: Date.now() }, composite);
  dirty = true;
  show();
  clearTimeout(timer);
  timer = setTimeout(save, QUIET);
}

// Send the whole record. One save at a time; steps that arrive meanwhile go
// out with the next.
async function save() {
  if (saving || !dirty || !piece) return saving;
  dirty = false;
  const sent = piece;
  saving = (async () => {
    try {
      if (!session.code) {
        const made = await call({ action: "create", id: session.id, key: session.key, width: SIDE, height: SIDE, initialLayers: 1 });
        session.code = made.code; session.revision = made.revision;
      }
      const encoded = await encodePaintingState(sent);
      if (encoded.length > MAX_STATE) throw new Error("this painting has more steps than can be kept");
      const kept = await call({ action: "save", code: session.code, key: session.key, revision: session.revision, state: encoded });
      session.revision = kept.revision; session.saved = sent.layers.length - 1; failed = null;
      remember({ id: session.id, key: session.key, code: session.code, hash: session.hash });
    } catch (error) {
      failed = error.message; dirty = true; // try again with the next step
    }
    saving = null;
    show();
    if (dirty && !failed) timer = setTimeout(save, QUIET);
  })();
  return saving;
}
state.save = save;

// ---- the page ---------------------------------------------------------------
const chip = document.createElement("div");
chip.id = "nopaint-steps";
chip.hidden = true;
chip.title = "This painting on Aesthetic Computer, and how many steps it has";
const style = document.createElement("style");
style.textContent = `#nopaint-steps { position: fixed; right: 6px; bottom: 6px; z-index: 9; padding: 2px 6px; font: 11px/1.3 monospace;
  color: #fff; background: #181820; opacity: 0.55; user-select: all; pointer-events: auto; }
  #nopaint-steps[data-saved="true"] { opacity: 0.9; } #nopaint-steps[hidden] { display: none; }`;
document.head.append(style);
(document.body || document.documentElement).append(chip);

channel.addEventListener("message", ({ data }) => {
  if (!data || data.to !== "page") return;
  if (data.id && waiting.has(data.id)) {
    const { resolve, reject } = waiting.get(data.id);
    waiting.delete(data.id);
    return data.error ? reject(new Error(data.error)) : resolve(data.result);
  }
  if (!data.event) return;
  state.events.push(data.event);
  // Steps are taken in the order the game reports them.
  if (data.event === "painted") queue = queue.then(() => step(data, "paint")).catch((error) => { failed = error.message; });
  if (data.event === "refused") queue = queue.then(() => step(data, "no")).catch((error) => { failed = error.message; });
  // The title screen means the painting on the canvas may be another one.
  if (data.event === "layout" && data.layout === "Splash") queue = queue.then(save).then(() => { piece = null; session = null; show(); });
});
let queue = Promise.resolve();
addEventListener("pagehide", () => { clearTimeout(timer); save(); });
