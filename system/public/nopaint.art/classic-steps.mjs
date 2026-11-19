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
// Painting takes a handle. A guest can look at the title screen and follow its
// links, but the first tap that would start a painting goes to sign-in, and
// every painting belongs to the handle that made it.
// Sign-in is AC's own turnstile, the hosted page at hi.aesthetic.computer
// (where a new person also picks a handle); this page has no form of its own.
//
// Nothing here is drawn in the page. What the player sees is the game's: its
// title screen's label says Sign In or the handle, in the game's own letters,
// and a tap on it is taken here before the game sees it. While painting, the
// handle sits in the checkerboard to the left of No and Paint.
//
// AI turns. The tuning (toolchain/nopaint/classic-tuning.json) can give an AI
// brush a share of the proposals. An AI turn is Note Paint's move, through the
// same service and the same Braincells as Aesel (/api/nopaint-inference): the
// painting and a short note go up, a new picture comes back, and the game
// shows it as a proposal to answer No or Paint. No Paint takes no input, so
// the game writes the note. A guest has the service's one free turn. A
// signed-in painter pays Braincells for each turn, so theirs are off until
// they turn them on.
//
// Loaded by index.html. window.nopaintSteps shows the state for debugging.

import { createNoPaintPiece, createNoPaintProposalLayer, appendNoPaintLayer, recoverNoPaintPiece }
  from "https://aesthetic.computer/aesthetic.computer/lib/nopaint-pieces.mjs";
import { encodePaintingState, decodePaintingState } from "https://aesthetic.computer/aesthetic.computer/lib/painting-state.mjs";

const API = "https://aesthetic.computer/api/painting-wip";
const AUTH = { domain: "hi.aesthetic.computer", clientId: "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt",
  sdk: "https://aesthetic.computer/aesthetic.computer/dep/auth0-spa-js.production.js" };
const HANDLE = "https://aesthetic.computer/api/handle";
const STORE = "nopaint:classic:steps";
const ACCOUNT = "nopaint:classic:account";
const MOVES = "https://aesthetic.computer/api/nopaint-inference";
const TRIAL_ID = "nopaint:classic:trial-id", TRIAL_USED = "nopaint:classic:trial-used", AI_ON = "nopaint:classic:ai-on";
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

// Who is painting: { label, sub } once signed in through the turnstile, or
// null for a guest. (A session handed over another way, { token, label } under
// ACCOUNT in localStorage, is honoured too: the live test signs in so.)
let account = (() => { try { return JSON.parse(localStorage.getItem(ACCOUNT) || "null"); } catch { return null; } })();
// Checking who is here takes a moment (the turnstile's library, then AC for
// the handle). Until it is done the title screen must not say Sign In to
// someone who is signed in. So the handle from last time is shown at once,
// and someone just back from the turnstile sees no label until theirs is known.
const WHO = "nopaint:classic:who";
const returning = (() => { const params = new URLSearchParams(location.search); return params.has("code") && params.has("state"); })();
const remembers = (() => { try { return Object.keys(localStorage).some((key) => key.startsWith("@@auth0spajs@@")); } catch { return false; } })();
let arriving = !account && (returning || remembers);
if (arriving && !returning) { try { const label = localStorage.getItem(WHO); if (label) account = { label, unchecked: true }; } catch {} }
let auth = null; // the turnstile's client, loaded when first needed
async function turnstile() {
  if (auth) return auth;
  if (!globalThis.auth0) await new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.src = AUTH.sdk; script.onload = resolve; script.onerror = () => reject(new Error("Sign-in could not load."));
    document.head.append(script);
  });
  return (auth = new globalThis.auth0.Auth0Client({ domain: AUTH.domain, clientId: AUTH.clientId, cacheLocation: "localstorage",
    useRefreshTokens: true, authorizationParams: { redirect_uri: location.origin }, httpTimeoutInSeconds: 30 }));
}
// The token to send as this painter, fresh each time, or null for a guest.
async function bearer() {
  await arrived;
  if (!account) return null;
  if (account.token) return account.token;
  try { return await (await turnstile()).getTokenSilently(); } catch { signedOut(); return null; }
}
function signedOut() {
  account = null;
  try { localStorage.removeItem(ACCOUNT); localStorage.removeItem(WHO); } catch {}
  auth?.logout({ openUrl: false }).catch(() => {});
}
// Coming back from the turnstile, or coming back signed in from an earlier visit.
async function arrive() {
  if (!arriving) return;
  try {
    const client = await turnstile();
    if (returning) try { await client.handleRedirectCallback(); } finally { history.replaceState(null, "", location.pathname); }
    if (!(await client.isAuthenticated())) { account = null; try { localStorage.removeItem(WHO); } catch {} return; }
    const user = await client.getUser();
    const handle = await fetch(`${HANDLE}?for=${encodeURIComponent(user.sub)}`).then((r) => r.json()).then((v) => v.handle, () => null);
    account = { sub: user.sub, label: handle ? `@${handle}` : account?.label || "signed in" };
    try { localStorage.setItem(WHO, account.label); } catch {}
  } catch (error) { failed = `sign-in: ${error.message}`; if (account?.unchecked) account = null; }
  finally { arriving = false; }
}

async function call(body) {
  const token = await bearer();
  const response = await fetch(API, { method: "POST", headers: { "Content-Type": "application/json", ...(token ? { Authorization: `Bearer ${token}` } : {}) }, body: JSON.stringify(body) });
  const value = await response.json().catch(() => ({}));
  if (!response.ok) throw Object.assign(new Error(value.error || `painting service answered ${response.status}`), { status: response.status });
  return value;
}

// ---- one painting -----------------------------------------------------------
// session: what this browser remembers. piece: the layers, in memory.
let session = null, piece = null, saving = null, timer = null, dirty = false, failed = null;
const state = (window.nopaintSteps = { get code() { return session?.code || null; }, get version() { return piece ? piece.layers.length - (session?.initial || 1) : 0; },
  get account() { return account?.label || null; }, log: [],
  get painting() { return session?.code ? `#${session.code} v${piece ? piece.layers.length - (session.initial || 1) : 0}` : null; },
  get title() { return { ...labels, spots: spots.map((spot) => spot.act) }; },
  get handleLine() { return handleLine; },
  get titleLine() { return aiLine; },
  get saved() { return session?.saved ?? 0; }, get error() { return failed; }, events: [] });
let told; // the #code the game was last told Save should show
const show = () => {
  // Save shows the painting's #code once it has one, a classic code until then.
  if ((session?.code || null) !== told) { told = session?.code || null; ask("code", { code: told }).catch(() => { told = undefined; }); }
  title();
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
  await arrived;
  if (!piece) await begin();
  const score = { kind: event.tool, engine: "nopaint-classic", options: event.options || "", decision,
    line: event.options ? `${event.tool}: ${event.options}` : event.tool, ...(receipts.get(event.turn) ? { ai: receipts.get(event.turn) } : {}) };
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

// ---- AI turns ---------------------------------------------------------------
const stored = (key) => { try { return localStorage.getItem(key); } catch { return null; } };
const store = (key, value) => { try { localStorage.setItem(key, value); } catch {} };
const receipts = new Map(); // turn id → { model, braincells }
// What happened lately, for looking at when something goes wrong: window.nopaintSteps.log
const log = (text) => { state.log.push(`${new Date().toISOString().slice(11, 23)} ${text}`); if (state.log.length > 60) state.log.shift(); };
let settings = null, offers = null, tired = false; // tired: a turn failed; no more this visit
// The brains this painter can use at the painting's size: the free one, and the cheapest paid one.
async function catalog() {
  const response = await fetch(MOVES, { cache: "no-store" });
  if (!response.ok) throw new Error("the AI service did not answer");
  const value = await response.json();
  const fits = (value.models || []).filter((model) => model.available && model.size?.[0] === SIDE && model.size?.[1] === SIDE);
  offers = {
    guest: value.trial ? fits.find((model) => (model.family || model.id) === value.trial.family) || null : null,
    paid: fits.toSorted((a, b) => a.braincells - b.braincells)[0] || null,
  };
}
const aiOffer = () => tired || !settings ? null
  : account ? (stored(AI_ON) === "true" ? offers?.paid : null)
  : settings.guests && stored(TRIAL_USED) !== "true" ? offers?.guest : null;
// Tell the game whether an AI turn can be had, so it is offered only then.
async function raise() {
  try {
    if (!settings) settings = (await ask("settings")).ai;
    const wanted = settings && !tired && (account ? stored(AI_ON) === "true" : settings.guests && stored(TRIAL_USED) !== "true");
    if (wanted && !offers) await catalog().catch(() => {});
    const answer = await ask("flags", { flags: { ai: Boolean(aiOffer()) } });
    log(`ai flag ${answer.flags.ai ? "up" : "down"}${tired ? " (tired)" : ""}`);
  } catch (error) { log(`raise failed: ${error.message}`); }
  show();
}
async function png(pixels) {
  const canvas = new OffscreenCanvas(SIDE, SIDE);
  canvas.getContext("2d").putImageData(new ImageData(new Uint8ClampedArray(pixels), SIDE, SIDE), 0, 0);
  const bytes = new Uint8Array(await (await canvas.convertToBlob({ type: "image/png" })).arrayBuffer());
  let binary = "";
  for (let i = 0; i < bytes.length; i += 32768) binary += String.fromCharCode(...bytes.subarray(i, i + 32768));
  return btoa(binary);
}
// One AI turn: the painting and the game's note go up; the picture goes to the game.
async function aiTurn({ id, note, settings: turn = {} }) {
  const offer = aiOffer(), guest = !account;
  log(`ai turn ${id} "${note}" ${offer ? offer.id : "no offer"}${guest ? " guest" : ""}`);
  try {
    if (!offer) throw new Error("no AI turn can be had");
    let trial = stored(TRIAL_ID);
    if (guest && !trial) store(TRIAL_ID, (trial = crypto.randomUUID()));
    const response = await fetch(MOVES, { method: "POST",
      headers: { "Content-Type": "application/json", ...(guest ? {} : { Authorization: `Bearer ${await bearer()}` }) },
      body: JSON.stringify({ requestId: crypto.randomUUID(), strength: [0.25, 0.5, 0.75].includes(turn.strength) ? turn.strength : 0.5,
        seed: Math.floor(Math.random() * 2 ** 31), hint: note, image: await png((await painting()).pixels), quote: offer.quote, model: offer.id,
        ...(guest ? { trialId: trial } : {}) }) });
    const value = await response.json().catch(() => ({}));
    if (!response.ok || typeof value.image !== "string") throw Object.assign(new Error(value.error || `the AI service answered ${response.status}`), { status: response.status, code: value.code });
    if (guest) store(TRIAL_USED, "true");
    receipts.set(id, { model: offer.id, braincells: value.billing?.braincells ?? 0 });
    log(`ai turn ${id} got a picture`);
    await ask("aiResult", { turn: id, png: Uint8Array.from(atob(value.image), (c) => c.charCodeAt(0)) });
    log(`ai turn ${id} handed to the game`);
  } catch (error) {
    log(`ai turn ${id} failed: ${error.message}`);
    // A guest's turn is spent or refused; a painter's sign-in or Braincells ran out; or the service is down. Stop asking.
    if (guest && error.status >= 400 && error.status < 500) store(TRIAL_USED, "true");
    if (error.status === 401) signedOut();
    tired = true;
    failed = `AI: ${error.message}`;
    await ask("aiResult", { turn: id, error: error.message }).catch(() => {});
  }
  raise();
}

// ---- the title screen -------------------------------------------------------
// The game's title screen has two states. It opens dark, with one small yellow
// label bottom right ("EARLY ACCESS" in 2021); a tap turns the lights on, and
// the lit screen has a slot top left ("Download App" in 2021). Whichever shows
// is the sign-in: Sign In for a guest, the handle for a painter. On the lit
// screen a second line under it turns a painter's AI turns on and off. All of
// it is the game's own sprite font, written through the bridge, and a tap on
// any of it is taken here before the game takes it.
const LIT = 114, DARK = 98;          // UIDs in the Construct source (layout Splash)
const LETTER = { [LIT]: 8.4, [DARK]: 9.7 }; // each label's letter advance, in layout pixels
let layoutNow = null, spots = [], labels = { who: null, ai: null, lights: null }, aiLine = null, asking = 0, titling = Promise.resolve(), watching = null;
function title() { titling = titling.then(drawTitle, drawTitle); return titling; }
async function drawTitle() {
  if (layoutNow !== "Splash") { spots = []; labels = { who: null, ai: null, lights: null }; aiLine = null; clearInterval(watching); watching = null; return; }
  // The lights come on without the layout changing, so keep looking while here.
  watching ||= setInterval(title, 600);
  try {
    const sure = account && Date.now() - asking < 4000;
    // Not known yet who is here: say nothing, and take no taps.
    const was = labels.who;
    labels.who = sure ? "Sign Out?" : account ? account.label : arriving ? "" : "Sign In";
    if (labels.who !== was) log(`title: ${labels.who || "(nothing yet)"}`);
    const dark = await ask("write", { uid: DARK, text: labels.who.toUpperCase(), fit: LETTER[DARK] });
    // A guest's Sign In is the loudest thing on the lit screen: twice the size,
    // yellow on black, as the game marks what to type. A handle sits quietly.
    // The slot is the screen's top left corner; every write puts it back there.
    const lit = await ask("write", labels.who === "Sign In"
      ? { uid: LIT, text: "[scale=2][background=#000000][color=#ffff00] Sign In [/color][/background][/scale]", x: 0, y: 0, width: 160, height: 32 }
      : { uid: LIT, text: labels.who, x: 0, y: 0, width: Math.max(100, Math.ceil(labels.who.length * LETTER[LIT])), height: 16 });
    labels.lights = !dark.shown;
    const next = labels.who ? [{ act: account ? "out" : "in", box: (dark.shown ? dark : lit).css }] : [];
    if (account && settings && !dark.shown) {
      const on = stored(AI_ON) === "true";
      labels.ai = on ? `AI On${offers?.paid ? ` ${offers.paid.braincells}` : ""}` : "AI Off";
      if (!aiLine) aiLine = (await ask("create", { like: LIT, y: lit.y + lit.height + 1, text: labels.ai })).uid;
      next.push({ act: "ai", box: (await ask("write", { uid: aiLine, text: labels.ai, visible: true, fit: LETTER[LIT] })).css });
    } else {
      if (aiLine) await ask("write", { uid: aiLine, visible: false }).catch(() => {});
      labels.ai = null;
    }
    spots = next;
  } catch (error) { spots = []; log(`title: ${error.message}`); }
}
async function act(what) {
  log(`tap: ${what}`);
  if (what === "in") {
    clearTimeout(timer);
    await save(); // the steps so far, before the page leaves for the turnstile
    return (await turnstile()).loginWithRedirect().catch((error) => { failed = `sign-in: ${error.message}`; });
  }
  if (what === "out") {
    // Two taps sign out: the first asks.
    if (Date.now() - asking < 4000) { asking = 0; signedOut(); return raise(); }
    asking = Date.now();
    setTimeout(() => { if (asking && Date.now() - asking >= 4000) { asking = 0; title(); } }, 4100);
    return title();
  }
  if (what === "ai") { store(AI_ON, stored(AI_ON) === "true" ? "false" : "true"); tired = false; return raise(); }
}
// ---- painting takes a handle ------------------------------------------------
// The game leaves its title screen for the Index layout to propose a brush.
// A guest who gets there is sent back and on to the turnstile.
async function gate() {
  await arrived;
  if (account) return;
  log("a guest tried to paint: to sign-in");
  await ask("goto", { layout: "Splash" }).catch(() => {});
  act("in");
}
// While painting on a desktop, the game's bottom strip has a checkerboard to
// the left of No and Paint. The painter's handle goes there, in the game's
// letters. (A phone's buttons fill the strip; there is no checkerboard.)
let handleLine = null;
async function sign() {
  handleLine = null;
  if (!account) return;
  try {
    if (!(await ask("state")).desktop) return;
    handleLine = (await ask("create", { type: "MainSpriteFont", layer: "UI", x: 3, y: 290, width: 122, height: 12, text: `[color=#000000]${account.label}[/color]` })).uid;
  } catch (error) { log(`handle line: ${error.message}`); }
}

// A tap inside a label never reaches the game.
let held = false;
const inside = (event) => spots.find(({ box }) => event.clientX >= box[0][0] && event.clientX <= box[1][0] && event.clientY >= box[0][1] && event.clientY <= box[1][1]);
addEventListener("pointerdown", (event) => {
  const spot = inside(event);
  if (!spot) return;
  held = true;
  event.stopImmediatePropagation(); event.preventDefault();
  act(spot.act);
}, true);
for (const type of ["pointerup", "pointercancel", "mousedown", "mouseup", "click", "touchstart", "touchend"]) addEventListener(type, (event) => {
  if (!held) return;
  event.stopImmediatePropagation(); event.preventDefault();
  if (type === "pointerup" || type === "pointercancel") setTimeout(() => { held = false; }, 0);
}, true);
addEventListener("resize", () => title());
state.signOut = () => { signedOut(); return raise(); };
state.act = act;
// Know who is here before the first save, so a painter's painting starts as theirs.
const arrived = arrive().then(() => raise());

channel.addEventListener("message", ({ data }) => {
  if (!data || data.to !== "page") return;
  if (data.id && waiting.has(data.id)) {
    const { resolve, reject } = waiting.get(data.id);
    waiting.delete(data.id);
    return data.error ? reject(new Error(data.error)) : resolve(data.result);
  }
  if (!data.event) return;
  state.events.push(data.event);
  if (data.event === "layout" || data.event === "ready") { layoutNow = data.layout; title(); }
  if (data.event === "layout" && data.layout === "Index") gate();
  if (data.event === "proposed") sign();
  // Steps are taken in the order the game reports them.
  if (data.event === "painted") queue = queue.then(() => step(data, "paint")).catch((error) => { failed = error.message; });
  if (data.event === "refused") queue = queue.then(() => step(data, "no")).catch((error) => { failed = error.message; });
  if (data.event === "ai") aiTurn(data);
  // Save shows this painting's #code: make sure AC has its latest step.
  if (data.event === "saving") { clearTimeout(timer); save(); }
  // Once the game is on its way, find out whether an AI turn can be had.
  if (data.event === "layout" && !settings && !raising) { raising = true; raise(); }
  // The title screen means the painting on the canvas may be another one.
  if (data.event === "layout" && data.layout === "Splash") queue = queue.then(save).then(() => { piece = null; session = null; show(); });
});
let queue = Promise.resolve(), raising = false;
addEventListener("pagehide", () => { clearTimeout(timer); save(); });
