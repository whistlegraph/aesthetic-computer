// 🎯 Dool Sheet
// The prompt as the paper over the dool world. This page is the host: it
// keeps a same-origin frame of dool (the guest, /dool/?embed=prompt) beneath
// the stage as an underlay, showing through the prompt's clear curtain from
// the first frame, and one number, how far the paper has faded, dissolves
// the stage out of the way and back over it.
//
// Down: a tap on the prompt's crosshair (`show` from the prompt piece, once
//       the reticle has flown to the centre).
// Up:   Escape in the world with nothing open and the mouse free, a tap on
//       the guest's corner block (`leave` from the guest), or Back.
//
//   cold → parked → moving → open → moving → parked
//
// The prompt's loop pauses the moment the paper starts moving and resumes
// once it is home again. The guest takes no seat until it is first opened,
// so a visit to the homepage never puts a player in the world.

const PAPER = "#141720"; // dool's own dark, behind a guest still loading
const GUEST = "/dool/?embed=prompt";

let frame;
let state = "cold"; // cold | parked | moving | open
let pausedLoop = false;
let historyEntry = false; // We pushed a /dool/ entry for the open sheet.
let swallowPop = false;
let said = false; // The guest has run to the end of its script and said hello.
let pendingShow = 0; // A show asked for before the guest was ready.

const post = (message) =>
  frame?.contentWindow?.postMessage(message, location.origin);
// 📐 The prompt's pixel grid (bios sets window.acFrameGrid on every frame):
// the guest lays its pixels on it, so the world and the paper share one scale.
const grid = () => window.acFrameGrid || null;
const stage = () => document.getElementById("aesthetic-computer");

function ensure() {
  if (frame) return frame;
  frame = document.createElement("iframe");
  frame.src = GUEST;
  frame.title = "dool";
  frame.allow = "autoplay; fullscreen; pointer-lock; gamepad; clipboard-write";
  Object.assign(frame.style, {
    position: "fixed",
    inset: "0",
    width: "100%",
    height: "100%",
    border: "0",
    zIndex: "-1", // An underlay: beneath the stage, over the page's backdrop.
    background: PAPER,
    colorScheme: "normal",
    pointerEvents: "none",
  });
  document.body.append(frame);
  state = "parked";
  return frame;
}

// The curtain is clear from the start, so the guest goes in at once.
function warm() {
  ensure();
}

// 🔁 The prompt sleeps while the paper is away.
function pausePrompt() {
  if (pausedLoop || window.__acLoopPaused) return;
  pausedLoop = true;
  window.acPAUSE?.();
}
function resumePrompt() {
  if (!pausedLoop) return;
  pausedLoop = false;
  window.acRESUME?.();
}

// 🧭 History: an open world is one entry; Back closes it.
function enterHistory() {
  if (historyEntry) return;
  history.pushState({ acDoolSheet: true }, "", "/dool/");
  historyEntry = true;
}
function leaveHistory() {
  if (!historyEntry) return;
  historyEntry = false;
  swallowPop = true;
  history.back();
}

// Called by bios's popstate handler; true means the sheet took it.
function popstate(e) {
  if (swallowPop) {
    swallowPop = false;
    return true;
  }
  if (state === "open" || (state === "moving" && toward === "open")) {
    historyEntry = false;
    hide();
    return true;
  }
  if (e?.state?.acDoolSheet) {
    historyEntry = true; // Forward into a dool entry: open it again.
    show();
    return true;
  }
  return false;
}

// 📐 One number: how far the paper has faded, from 0 (home, opaque) to the
// screen's height (gone). `y` runs from where it was to `goal` over a fixed
// time with an ease-in-out, so both ways take the same short beat and end
// cleanly (2026-11-03: the exponential chase had a long faint tail).
let y = 0;
let goal = 0;
let toward = null; // A move in flight: "open" or "home".
const BEAT = 360; // ms, the whole move: the crosshair's flight takes the same
let loop = 0;
let lastStep = 0;
let from = 0, began = 0; // The move's start value and time.

function write() {
  const s = stage();
  if (s) s.style.opacity = y <= 0 ? "" : String(Math.max(0, 1 - y / innerHeight));
  window.acMeterPaint?.(); // A slide frame is a painted frame to the frame meter.
}

function run() {
  if (loop) return;
  lastStep = performance.now();
  from = y;
  began = lastStep;
  loop = requestAnimationFrame(step);
}

function run2() {
  if (loop) return;
  loop = requestAnimationFrame(step);
}

function step(now) {
  loop = 0;
  lastStep = now;
  const t = Math.min(1, (now - began) / BEAT);
  const ease = t < 0.5 ? 2 * t * t : 1 - 2 * (1 - t) * (1 - t);
  y = t >= 1 ? goal : from + (goal - from) * ease;
  write();
  if (y !== goal) return run2();
  if (toward === null) return;
  const target = toward;
  toward = null;
  target === "open" ? opened() : home();
}

function show() {
  const f = ensure();
  if (state === "open" || toward === "open") return;
  if (!said) {
    // Nothing to drop onto yet: wait a little for the guest, then let it go.
    clearTimeout(pendingShow);
    pendingShow = setTimeout(() => (pendingShow = 0), 4000);
    return;
  }
  if (state === "parked") post({ type: "dool:sheet", open: true });
  state = "moving";
  toward = "open";
  goal = innerHeight;
  pausePrompt(); // The last frame fades away; the loop is back at home().
  const s = stage();
  if (s) s.style.willChange = "opacity";
  run();
}

function hide() {
  if (!frame || state === "parked" || state === "cold" || toward === "home") return;
  state = "moving";
  toward = "home";
  goal = 0;
  frame.style.pointerEvents = "none";
  // Beneath the stage at once, so the paper fades in over the world (left on
  // top, the world hid the whole fade and then cut). The prompt wakes now:
  // its crosshair flies home while the guest's reticle flies the same path
  // under the fading paper, one cursor (2026-11-03).
  frame.style.zIndex = "-1";
  resumePrompt();
  post({ type: "dool:sheet", phase: "closing" }); // The HUD rests before the paper returns.
  run();
}

function opened() {
  state = "open";
  write();
  post({ type: "dool:sheet", phase: "opened" }); // The paper is gone: the guest may show its HUD.
  pausePrompt();
  enterHistory();
  // An underlay sits beneath the body for hit testing, so the open world
  // comes forward to take clicks; the stage is off screen anyway.
  frame.style.zIndex = "1000";
  frame.style.pointerEvents = "auto";
  // No host cursor over the world: the guest hides its own, and the mouse
  // is usually captured, so nothing is left to flicker at the frame's edge.
  document.documentElement.style.cursor = "none";
  document.body.style.cursor = "none";
  frame.contentWindow?.focus();
  meterGuest();
}

// 📊 The Mac app's frame meter counts the piece's painted frames, which stop
// while the loop sleeps under the world. Count the guest's frames instead.
function meterGuest() {
  const guest = frame.contentWindow;
  if (!guest || !window.acMeterPaint) return;
  const generation = ++meterGeneration;
  const tick = () => {
    if (state !== "open" || generation !== meterGeneration) return;
    window.acMeterPaint();
    guest.requestAnimationFrame(tick);
  };
  guest.requestAnimationFrame(tick);
}
let meterGeneration = 0;

function home() {
  state = "parked";
  y = goal = 0;
  frame.style.pointerEvents = "none";
  frame.style.zIndex = "-1"; // Back beneath the stage for the paper to cover.
  const s = stage();
  if (s) Object.assign(s.style, { opacity: "", willChange: "" });
  document.documentElement.style.cursor = "";
  document.body.style.cursor = "";
  resumePrompt();
  post({ type: "dool:sheet", open: false }); // Park the guest: no input, no sound.
  leaveHistory();
  window.focus();
}

addEventListener("resize", () => {
  if (state !== "open") return;
  y = goal = innerHeight;
  write();
});

// Escape reaching this window while the world is open (focus wandered back
// to the host) still comes home.
addEventListener("keydown", (e) => {
  if (state === "open" && e.key === "Escape") hide();
});

// ⬆️ Up: the guest says when it is ready and when the player leaves.
addEventListener("message", (e) => {
  if (!frame || e.origin !== location.origin || e.source !== frame.contentWindow) return;
  const d = e.data || {};
  if (d.type === "dool:sheet:hello") {
    said = true;
    post({ type: "dool:sheet", open: state === "open" || toward === "open", grid: grid() });
    // A server restart reloads the guest without moving the host's paper.
    // Restore the completed transition too: otherwise its HUD stays hidden
    // and the native frame meter is still attached to the discarded document.
    if (state === "open") {
      post({ type: "dool:sheet", phase: "opened" });
      meterGuest();
    }
    if (pendingShow) {
      clearTimeout(pendingShow);
      pendingShow = 0;
      show();
    }
  }
  else if (d.type === "dool:sheet:leave") hide();
});

// bios reframed: pass the new grid down.
function framed() {
  if (said) post({ type: "dool:sheet", grid: grid() });
}

window.acDoolSheet = { popstate, framed };

// Messages from the prompt piece:
//   { action: "warm" }   load the guest on idle
//   { action: "show" }   drop the paper into the world
//   { action: "hide" }   bring the paper back
export function doolSheet({ action, value } = {}) {
  if (action === "warm") warm();
  else if (action === "show") show();
  else if (action === "hide") hide();
  // The crosshair's breadth as the pointer nears: the room scales with it.
  else if (action === "near") post({ type: "dool:sheet", near: value });
}
