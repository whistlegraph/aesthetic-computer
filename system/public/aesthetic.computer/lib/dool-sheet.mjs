// 🎯 Dool Sheet
// The prompt as the paper over the dool world. This page is the host: it
// keeps a same-origin frame of dool (the guest, /dool/?embed=prompt) parked
// beneath the stage as an underlay, and one number, how far the paper has
// dropped, moves the stage down to uncover it and back up to cover it.
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
    transform: "translateY(100%)", // Parked off screen, clear of the gaps around the stage.
    pointerEvents: "none",
  });
  document.body.append(frame);
  state = "parked";
  return frame;
}

function warm() {
  if (frame) return;
  const go = () => ensure();
  if (window.requestIdleCallback) requestIdleCallback(go, { timeout: 8000 });
  else setTimeout(go, 3000);
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

// 📐 One position: the paper's top edge at `y`, from 0 (home) to the screen's
// height (dropped). `y` chases `goal` through a short time constant, so the
// drop and the rise both ease out over about half a second.
let y = 0;
let goal = 0;
let toward = null; // A move in flight: "open" or "home".
const TAU = 70; // ms to close 63% of the gap
let loop = 0;
let lastStep = 0;

function write() {
  const s = stage();
  if (s) s.style.transform = y <= 0 ? "" : `translateY(${y}px)`;
  window.acMeterPaint?.(); // A slide frame is a painted frame to the frame meter.
}

function run() {
  if (loop) return;
  lastStep = performance.now();
  loop = requestAnimationFrame(step);
}

function step(now) {
  loop = 0;
  const dt = Math.min(64, now - lastStep);
  lastStep = now;
  const gap = goal - y;
  y = Math.abs(gap) < 1 ? goal : y + gap * (1 - Math.exp(-dt / TAU));
  write();
  if (y !== goal) return run();
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
  if (state === "parked") {
    f.style.transform = ""; // Into place beneath the stage before the paper moves.
    post({ type: "dool:sheet", open: true });
  }
  state = "moving";
  toward = "open";
  goal = innerHeight;
  pausePrompt(); // The last frame rides along; the loop is back at home().
  const s = stage();
  if (s) s.style.willChange = "transform";
  run();
}

function hide() {
  if (!frame || state === "parked" || state === "cold" || toward === "home") return;
  state = "moving";
  toward = "home";
  goal = 0;
  frame.style.pointerEvents = "none";
  run();
}

function opened() {
  state = "open";
  write();
  pausePrompt();
  enterHistory();
  frame.style.pointerEvents = "auto";
  frame.contentWindow?.focus();
  meterGuest();
}

// 📊 The Mac app's frame meter counts the piece's painted frames, which stop
// while the loop sleeps under the world. Count the guest's frames instead.
function meterGuest() {
  const guest = frame.contentWindow;
  if (!guest || !window.acMeterPaint) return;
  const tick = () => {
    if (state !== "open") return;
    window.acMeterPaint();
    guest.requestAnimationFrame(tick);
  };
  guest.requestAnimationFrame(tick);
}

function home() {
  state = "parked";
  y = goal = 0;
  Object.assign(frame.style, {
    transform: "translateY(100%)",
    pointerEvents: "none",
  });
  const s = stage();
  if (s) Object.assign(s.style, { transform: "", willChange: "" });
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
    post({ type: "dool:sheet", open: state === "open" || toward === "open" });
    if (pendingShow) {
      clearTimeout(pendingShow);
      pendingShow = 0;
      show();
    }
  }
  else if (d.type === "dool:sheet:leave") hide();
});

window.acDoolSheet = { popstate };

// Messages from the prompt piece:
//   { action: "warm" }   load the guest on idle
//   { action: "show" }   drop the paper into the world
//   { action: "hide" }   bring the paper back
export function doolSheet({ action } = {}) {
  if (action === "warm") warm();
  else if (action === "show") show();
  else if (action === "hide") hide();
}
