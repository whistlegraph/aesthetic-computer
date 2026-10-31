// 🎭 Mime Sheet
// The prompt as the first post of the mime feed. This page is the host: it
// keeps a same-origin frame of /mime/?embed=prompt (the guest) parked below
// the screen, and one engine moves both ways. A single number, the feed's top
// edge on screen, places the frame there and the prompt a screen above it,
// written in the same frame, so the two never drift apart.
//
// Down:  a curtain swipe (`grab` from the prompt piece), two-finger scroll or
//        the down arrow on the prompt.
// Up:    the guest, sitting on its first card at the top of its own scroll,
//        forwards pulls, wheel deltas and the up arrow (`pull`, `wheel`, `back`).
// Both:  follow the input, then settle on release: past half the screen the
//        trip finishes, anything less eases back to where it was. No flicks,
//        no notches that jump: the sheet is wherever the input brought it.
//
//   cold → parked → moving → open → moving → parked
//
// The prompt's loop pauses the moment the sheet starts moving and resumes
// only once it is home again, so the slide spends the main thread on the
// slide. The guest frame stays visible and promoted while parked (translated
// a screen down), so its first appearance costs no paint. The address bar
// follows the card (/mime/?p=<code>) and Back closes.

const MEDIA = "#171526"; // mime's dark media colour, behind a guest still loading
const COMMIT = 0.5; // of the screen: the halfway line decides, nothing else

let frame;
let state = "cold"; // cold | parked | moving | open
let pausedLoop = false;
let lastDownY = null; // clientY of the latest pointerdown, for an exact grab.
let historyEntry = false; // We pushed a /mime/?p= entry for the open sheet.
let swallowPop = false;
let lastPermalink = null; // The guest's card on screen, as last announced.
let lastWheelAt = 0;
let armed = false; // The prompt says its curtain is up and empty.

const post = (message) =>
  frame?.contentWindow?.postMessage(message, location.origin);
const stage = () => document.getElementById("aesthetic-computer");

addEventListener("pointerdown", (e) => (lastDownY = e.clientY), {
  capture: true,
  passive: true,
});

function ensure() {
  if (frame) return frame;
  frame = document.createElement("iframe");
  frame.src = "/mime/?embed=prompt";
  frame.title = "mime";
  frame.allow = "autoplay; fullscreen; clipboard-write";
  Object.assign(frame.style, {
    position: "fixed",
    inset: "0",
    width: "100%",
    height: "100%",
    border: "0",
    zIndex: "1000",
    background: MEDIA, // Never white; transparent once the guest is ready.
    colorScheme: "normal", // Matching schemes keep the frame see-through.
    transform: "translateY(100%)",
    willChange: "transform", // Promoted from the start: rastered before any swipe.
    pointerEvents: "none",
  });
  document.body.append(frame);
  state = "parked";
  return frame;
}

function warm() {
  if (frame) return;
  const go = () => ensure();
  if (window.requestIdleCallback) requestIdleCallback(go, { timeout: 4000 });
  else setTimeout(go, 1500);
}

// 🔁 The prompt sleeps while the feed covers it.
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

// 🧭 History: an open sheet is one entry; Back closes it.
function enterHistory() {
  const url = "/mime/" + (lastPermalink ? "?p=" + encodeURIComponent(lastPermalink) : "");
  if (!historyEntry) {
    history.pushState({ acMimeSheet: true }, "", url);
    historyEntry = true;
  } else history.replaceState({ acMimeSheet: true }, "", url);
}
function leaveHistory() {
  if (!historyEntry) return;
  historyEntry = false;
  swallowPop = true;
  history.back();
}

// Called first by bios's popstate handler; true means the sheet took it.
function popstate(e) {
  if (swallowPop) {
    swallowPop = false;
    return true;
  }
  if (state === "open" || (state === "moving" && toward === 0)) {
    historyEntry = false;
    settle(innerHeight);
    return true;
  }
  if (e?.state?.acMimeSheet) {
    historyEntry = true; // Forward into a sheet entry: open it again.
    settle(0);
    return true;
  }
  return false;
}

// 📐 One position for both: the feed's top edge at `y`, the prompt above it.
// `y` chases `goal` every frame through a short time constant, so whatever the
// input does — a finger, a trackpad stream and its momentum tail, a mouse
// notch, a release — the sheet itself only ever moves smoothly. Live input
// sits a hair behind the finger; a settle eases out over about half a second.
let y = innerHeight;
let goal = innerHeight;
let toward = null; // A settle in flight: 0 (open) or innerHeight (home).
let tau = 40; // ms to close 63% of the gap
const FOLLOW = 40;
const GLIDE = 70;
let loop = 0;
let lastStep = 0;

function write() {
  const s = stage();
  frame.style.transform = `translateY(${y}px)`;
  if (s) s.style.transform = y >= innerHeight ? "" : `translateY(${y - innerHeight}px)`;
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
  y = Math.abs(gap) < 1 ? goal : y + gap * (1 - Math.exp(-dt / tau));
  write();
  if (y !== goal) return run();
  if (toward === null) return;
  const target = toward;
  toward = null;
  target === 0 ? opened() : home();
}

function still() {
  cancelAnimationFrame(loop);
  loop = 0;
}

// Follow live input: the goal moves, the sheet chases it.
function follow(next) {
  goal = Math.max(0, Math.min(innerHeight, next));
  tau = FOLLOW;
  run();
}

// Begin moving from wherever things are, a glide in progress included.
function begin() {
  const f = ensure();
  if (state === "parked") {
    post({ type: "mime:sheet", open: true }); // First card up, slot away.
    y = goal = innerHeight;
  }
  state = "moving";
  toward = null;
  pausePrompt(); // The last frame rides along; the loop is back at home().
  const s = stage();
  if (s) s.style.willChange = "transform";
}

// Glide to 0 (open) or innerHeight (home) from wherever the sheet is now.
function settle(target) {
  const f = ensure();
  if (state === "parked" && target === 0) begin();
  state = "moving";
  toward = target;
  f.style.pointerEvents = "none";
  goal = target;
  tau = GLIDE;
  run();
}

function opened() {
  state = "open";
  write();
  pausePrompt();
  enterHistory();
  meterGuest();
  // Take input once a trackpad's momentum has died down, or its tail would
  // scroll straight past the first card.
  const take = () => {
    if (state !== "open") return;
    if (performance.now() - lastWheelAt < 220) return setTimeout(take, 60);
    frame.style.pointerEvents = "auto";
    frame.contentWindow?.focus();
  };
  take();
}

// 📊 The Mac app's frame meter counts the piece's painted frames, which stop
// while the loop sleeps under the sheet. Over mime the frames on screen are
// the guest's: count its animation frames instead, so the meter reads the
// feed rather than zero.
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
  still();
  y = goal = innerHeight;
  Object.assign(frame.style, {
    transform: "translateY(100%)",
    pointerEvents: "none",
  });
  const s = stage();
  if (s) Object.assign(s.style, { transform: "", willChange: "" });
  resumePrompt();
  post({ type: "mime:sheet", open: false }); // Park the guest on its slot.
  leaveHistory();
  window.focus();
}

// Release: past the halfway line the trip finishes, short of it the sheet
// eases back where it came from. `travel` is how far toward the other end
// the input went.
function release(from, travel) {
  const done = travel >= innerHeight * COMMIT;
  if (from === "home") settle(done ? 0 : innerHeight);
  else settle(done ? innerHeight : 0);
}

// ✋ Down: a curtain swipe, handed over by the prompt piece once it engages.
function grab(startY) {
  if (state !== "parked" && state !== "cold") return;
  begin();
  const y0 = lastDownY ?? startY;
  let last = y0;
  const move = (e) => {
    last = e.clientY;
    follow(innerHeight - (y0 - last));
  };
  const end = () => {
    removeEventListener("pointermove", move, true);
    removeEventListener("pointerup", end, true);
    removeEventListener("pointercancel", end, true);
    release("home", y0 - last);
  };
  addEventListener("pointermove", move, { capture: true, passive: true });
  addEventListener("pointerup", end, { capture: true, passive: true });
  addEventListener("pointercancel", end, { capture: true, passive: true });
}

// 🖱️ Wheels, either way. Every delta — a trackpad's stream and momentum, a
// mouse notch, a line or page step — is so much travel, and the sheet follows
// it; nothing jumps the sheet open. The stream ends after a short silence and
// the halfway line decides, or crossing it mid-stream finishes the trip.
// `dy` > 0 always means "toward the other end".
let wheel = null;

function wheelStep(from, dy, mode) {
  lastWheelAt = performance.now();
  const px = mode === 1 ? dy * 16 : mode === 2 ? dy * innerHeight : dy;
  if (!wheel) {
    if (px <= 0) return;
    begin(); // Mid-glide, this carries on from the live y.
    wheel = { from, travel: from === "home" ? innerHeight - y : y, timer: 0 };
  }
  wheel.travel = Math.max(0, wheel.travel + px);
  follow(wheel.from === "home" ? innerHeight - wheel.travel : wheel.travel);
  clearTimeout(wheel.timer);
  if (wheel.travel >= innerHeight * COMMIT) return endWheel();
  wheel.timer = setTimeout(() => endWheel(), 140);
}

function endWheel() {
  if (!wheel) return;
  clearTimeout(wheel.timer);
  const { from, travel } = wheel;
  wheel = null;
  release(from, travel);
}

addEventListener("wheel", (e) => {
  lastWheelAt = performance.now();
  if (!frame || !armed || e.ctrlKey) return; // ctrl+wheel is pinch-zoom
  if (Math.abs(e.deltaX) > Math.abs(e.deltaY)) return;
  // Parked, or easing home after a short pull: a new scroll picks it back up.
  const home = state === "parked" || (state === "moving" && toward === innerHeight);
  if (wheel?.from === "home" || (!wheel && home))
    wheelStep("home", e.deltaY, e.deltaMode);
}, { capture: true, passive: true });

// ⬆️ Up: the guest forwards what happens at the top of its first card.
let pull = false;

addEventListener("message", (e) => {
  if (!frame || e.origin !== location.origin || e.source !== frame.contentWindow) return;
  const d = e.data || {};
  if (d.type === "mime:sheet:hello") post({ type: "mime:sheet", open: state !== "parked" && state !== "cold" });
  else if (d.type === "mime:sheet:ready") frame.style.background = "transparent";
  else if (d.type === "mime:sheet:at") {
    lastPermalink = d.permalink || null;
    if (historyEntry) enterHistory();
  } else if (d.type === "mime:sheet:pull") {
    if (d.end) {
      if (pull) release("open", goal);
      pull = false;
    } else {
      if (!pull) {
        if (state !== "open") return;
        begin();
        pull = true;
      }
      follow(d.dy);
    }
  } else if (d.type === "mime:sheet:wheel") {
    if (wheel?.from === "open" || (!wheel && state === "open")) wheelStep("open", d.dy, d.mode);
  } else if (d.type === "mime:sheet:back") {
    if (state === "open") settle(innerHeight);
  }
});

window.acMimeSheet = { popstate };

// Messages from the prompt piece:
//   { action: "warm" }              load the guest on idle
//   { action: "grab", y, of }       a swipe began at y (piece px of `of` tall)
//   { action: "show" }              open without a drag (down arrow)
//   { action: "armed", on }         the curtain is up and empty (wheel pulls)
export function mimeSheet({ action, y: at, of, on } = {}) {
  if (action === "warm") warm();
  else if (action === "armed") armed = !!on;
  else if (action === "grab") grab(of ? (at * innerHeight) / of : innerHeight);
  else if (action === "show") {
    if (state === "parked" || state === "cold") settle(0);
  }
}
