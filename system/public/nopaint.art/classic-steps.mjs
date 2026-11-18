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
// A guest's painting is held by a key kept in this browser. Signing in with
// an Aesthetic Computer handle is offered, never asked for: the service then
// gives the painting to that handle, and later paintings start as theirs.
// Sign-in is Note Paint's (/api/nopaint-signin): a link sent by email, opened
// on any device, and this page learns of it by polling.
//
// Loaded by index.html. window.nopaintSteps shows the state for debugging.

import { createNoPaintPiece, createNoPaintProposalLayer, appendNoPaintLayer, recoverNoPaintPiece }
  from "https://aesthetic.computer/aesthetic.computer/lib/nopaint-pieces.mjs";
import { encodePaintingState, decodePaintingState } from "https://aesthetic.computer/aesthetic.computer/lib/painting-state.mjs";

const API = "https://aesthetic.computer/api/painting-wip";
const SIGNIN = "/api/nopaint-signin"; // same origin; lith answers it
const STORE = "nopaint:classic:steps";
const ACCOUNT = "nopaint:classic:account";
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

// Who is painting: { token, label } once signed in, or null for a guest.
let account = (() => { try { return JSON.parse(localStorage.getItem(ACCOUNT) || "null"); } catch { return null; } })();
function signedOut() { account = null; try { localStorage.removeItem(ACCOUNT); } catch {} }

async function call(body) {
  const response = await fetch(API, { method: "POST", headers: { "Content-Type": "application/json", ...(account?.token ? { Authorization: `Bearer ${account.token}` } : {}) }, body: JSON.stringify(body) });
  const value = await response.json().catch(() => ({}));
  if (!response.ok) throw Object.assign(new Error(value.error || `painting service answered ${response.status}`), { status: response.status });
  return value;
}

// ---- one painting -----------------------------------------------------------
// session: what this browser remembers. piece: the layers, in memory.
let session = null, piece = null, saving = null, timer = null, dirty = false, failed = null;
const state = (window.nopaintSteps = { get code() { return session?.code || null; }, get version() { return piece ? piece.layers.length - (session?.initial || 1) : 0; },
  get account() { return account?.label || null; },
  get saved() { return session?.saved ?? 0; }, get error() { return failed; }, events: [] });
const show = () => {
  chip.hidden = !session?.code;
  chip.textContent = session?.code ? `#${session.code} v${state.version}` : "";
  chip.dataset.saved = String(state.saved === state.version);
  who.textContent = account?.label || "sign in";
  who.dataset.signedIn = String(Boolean(account));
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
      if (read.canEdit && recovered) {
        const steps = Number.isInteger(read.steps) ? read.steps : recovered.layers.length - 1;
        session = { ...kept, initial: recovered.layers.length - steps, revision: read.revision, saved: steps };
        piece = recovered;
        return show();
      }
    } catch {}
  }
  session = { id: random(24), key: random(32), code: null, initial: 1, revision: 0, hash: now.hash, saved: 0 };
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
        const made = await call({ action: "create", id: session.id, key: session.key, width: SIDE, height: SIDE, initialLayers: session.initial,
          ...(session.parent ? { parent: session.parent } : {}) });
        session.code = made.code; session.revision = made.revision;
      }
      const encoded = await encodePaintingState(sent);
      if (encoded.length > MAX_STATE) throw new Error("this painting has more steps than can be kept");
      const kept = await call({ action: "save", code: session.code, key: session.key, revision: session.revision, state: encoded });
      session.revision = kept.revision; session.saved = sent.layers.length - session.initial; failed = null;
      remember({ id: session.id, key: session.key, code: session.code, hash: session.hash });
    } catch (error) {
      dirty = true; // try again with the next step
      if ((error.status === 401 || error.status === 403) && session.code) {
        // The sign-in ran out, or the painting is a handle's and this browser
        // is a guest again. Go on in a new painting that starts from this one.
        if (error.status === 401) signedOut();
        session = { id: random(24), key: random(32), code: null, parent: session.code, initial: sent.layers.length, revision: 0, hash: session.hash, saved: 0 };
        failed = null;
      } else failed = error.message;
    }
    saving = null;
    show();
    if (dirty && !failed) timer = setTimeout(save, QUIET);
  })();
  return saving;
}
state.save = save;

// ---- the page ---------------------------------------------------------------
const bar = document.createElement("div");
bar.id = "nopaint-steps-bar";
bar.innerHTML = `<button type="button" id="nopaint-who" title="Keep your paintings under an Aesthetic Computer handle"></button><span id="nopaint-steps" hidden title="This painting on Aesthetic Computer, and how many steps it has"></span>
<dialog id="nopaint-signin" aria-labelledby="nopaint-signin-title"><form method="dialog">
<h2 id="nopaint-signin-title">Keep your paintings</h2>
<p class="lede">Sign in with your Aesthetic Computer handle and this painting, with every step, is yours. We send a link to your email.</p>
<label>Email <input type="email" name="email" autocomplete="email" required></label>
<p class="status" role="status"></p>
<div class="row"><button value="cancel" formnovalidate>Close</button><button type="submit" value="send">Send link</button></div>
</form></dialog>`;
const style = document.createElement("style");
style.textContent = `#nopaint-steps-bar { position: fixed; right: 6px; bottom: 6px; z-index: 9; display: flex; gap: 4px; align-items: center; font: 11px/1.3 monospace; }
  #nopaint-steps, #nopaint-who { padding: 2px 6px; color: #fff; background: #181820; border: 0; border-radius: 0; font: inherit; }
  #nopaint-steps { opacity: 0.55; user-select: all; } #nopaint-steps[data-saved="true"] { opacity: 0.9; } #nopaint-steps[hidden] { display: none; }
  #nopaint-who { cursor: pointer; opacity: 0.9; } #nopaint-who[data-signed-in="true"] { color: #ffdd00; }
  #nopaint-signin { max-width: min(380px, calc(100vw - 32px)); width: 100%; padding: 0; border: 3px solid #181820; background: #fff; color: #181820; }
  #nopaint-signin::backdrop { background: rgba(24, 24, 32, .45); }
  #nopaint-signin form { display: grid; gap: 10px; padding: 16px; font: 15px/1.35 system-ui, sans-serif; }
  #nopaint-signin h2, #nopaint-signin p { margin: 0; } #nopaint-signin h2 { font-size: 19px; }
  #nopaint-signin label { display: grid; gap: 3px; font-size: 13px; }
  #nopaint-signin input { font: inherit; font-size: 16px; padding: 7px 8px; border: 2px solid #181820; border-radius: 0; }
  #nopaint-signin .row { display: flex; gap: 10px; justify-content: flex-end; }
  #nopaint-signin button { font: inherit; padding: 6px 12px; border: 2px solid #181820; background: #fff; color: #181820; cursor: pointer; border-radius: 0; }
  #nopaint-signin button[type=submit] { background: #181820; color: #fff; } #nopaint-signin .status { min-height: 1.2em; font-size: 13px; }`;
document.head.append(style);
(document.body || document.documentElement).append(bar);
const chip = bar.querySelector("#nopaint-steps"), who = bar.querySelector("#nopaint-who"), dialog = bar.querySelector("#nopaint-signin");
const form = dialog.querySelector("form"), status = dialog.querySelector(".status");

// ---- sign in ----------------------------------------------------------------
async function signin(body) {
  const response = await fetch(SIGNIN, { method: "POST", cache: "no-store", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) });
  const value = await response.json().catch(() => ({}));
  if (!response.ok) throw new Error(value.error || value.message || "Sign-in could not finish.");
  return value;
}
let pairing = null; // { code, pollSecret, until } while a link is out
async function waitForLink() {
  while (pairing && Date.now() < pairing.until) {
    await new Promise((done) => setTimeout(done, 2500));
    if (!pairing) return;
    const answer = await signin({ action: "poll", code: pairing.code, pollSecret: pairing.pollSecret }).catch(() => null);
    if (answer?.status !== "claimed" || !answer.session?.accessToken) continue;
    account = { token: answer.session.accessToken, label: answer.session.account?.label || "signed in" };
    try { localStorage.setItem(ACCOUNT, JSON.stringify(account)); } catch {}
    pairing = null;
    if (dialog.open) dialog.close();
    // Saving again as this handle makes the painting theirs.
    if (piece) { dirty = true; save(); }
    return show();
  }
  if (pairing) { pairing = null; status.textContent = "That link ran out. Send another."; }
}
// The game reads the keyboard everywhere; typing here must stay here. Its
// page script also takes every selection to start in an element, and throws
// when one starts in text, so those stop here too.
for (const type of ["keydown", "keyup", "keypress"]) dialog.addEventListener(type, (event) => event.stopPropagation());
bar.addEventListener("selectstart", (event) => event.stopPropagation());
who.addEventListener("click", () => {
  if (account) { if (confirmSignOut()) { signedOut(); show(); } return; }
  status.textContent = "";
  dialog.showModal();
});
// Two clicks sign out: the first asks.
let asking = 0;
function confirmSignOut() {
  if (Date.now() - asking < 4000) { asking = 0; return true; }
  asking = Date.now();
  who.textContent = "sign out?";
  setTimeout(() => { if (asking) { asking = 0; show(); } }, 4000);
  return false;
}
form.addEventListener("submit", async (event) => {
  if (event.submitter?.value !== "send") { pairing = null; return; }
  event.preventDefault();
  const email = form.elements.email.value.trim();
  status.textContent = "Sending…";
  try {
    const made = await signin({ action: "create" });
    const parameters = await signin({ action: "email", code: made.code, email });
    const sent = await fetch("https://hi.aesthetic.computer/passwordless/start", { method: "POST", headers: { "Content-Type": "application/json" },
      signal: AbortSignal.timeout(15000), body: JSON.stringify({ ...parameters, connection: "email", email, send: "link" }) });
    if (!sent.ok) throw new Error(sent.status === 429 ? "Please wait before asking for another link." : "The link could not be sent.");
    pairing = { code: made.code, pollSecret: made.pollSecret, until: Date.now() + 590_000 };
    status.textContent = "Check your email and open the link. Keep this page open.";
    waitForLink();
  } catch (error) { status.textContent = error.message; }
});
state.signOut = () => { signedOut(); show(); };
show();

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
