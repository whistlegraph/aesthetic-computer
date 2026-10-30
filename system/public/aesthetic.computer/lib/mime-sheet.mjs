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
// Both:  follow the input, then settle on release: past 15% of the screen or a
//        flick finishes the trip, anything less springs back.
//
//   cold → parked → moving → open → moving → parked
//
// The prompt's loop pauses the moment the sheet starts moving and resumes
// only once it is home again, so the slide spends the main thread on the
// slide. The guest frame stays visible and promoted while parked (translated
// a screen down), so its first appearance costs no paint. The address bar
// follows the card (/mime/?p=<code>) and Back closes.

const MEDIA = "#171526"; // mime's dark media colour, behind a guest still loading
const EASE = "cubic-bezier(.2,.8,.2,1)";
const COMMIT = 0.15; // of the screen
const FLICK = 0.45; // px/ms

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
let y = innerHeight;
let toward = null; // Where a settle is heading: 0 (open) or innerHeight (home).
let raf = 0;

function write(transition = "none") {
  const s = stage();
  frame.style.transition = transition;
  frame.style.transform = `translateY(${y}px)`;
  if (s) {
    s.style.transition = transition;
    s.style.transform = y >= innerHeight ? "" : `translateY(${y - innerHeight}px)`;
  }
}

// Follow live input: write at most once per frame.
function follow(next) {
  y = Math.max(0, Math.min(innerHeight, next));
  if (raf) return;
  raf = requestAnimationFrame(() => {
    raf = 0;
    write();
  });
}

// Begin moving from wherever things are.
function begin() {
  const f = ensure();
  cancelAnimationFrame(raf);
  raf = 0;
  if (state === "parked") {
    post({ type: "mime:sheet", open: true }); // First card up, slot away.
    y = innerHeight;
  }
  state = "moving";
  toward = null;
  pausePrompt(); // The last frame rides along; the loop is back at home().
  const s = stage();
  if (s) s.style.willChange = "transform";
}

// Glide to 0 (open) or innerHeight (home), quicker for shorter trips.
function settle(target) {
  const f = ensure();
  if (state === "parked" && target === 0) begin();
  cancelAnimationFrame(raf);
  raf = 0;
  state = "moving";
  toward = target;
  f.style.pointerEvents = "none";
  const ms = Math.round(120 + 220 * Math.min(1, Math.abs(target - y) / innerHeight));
  y = target;
  requestAnimationFrame(() => write(`transform ${ms}ms ${EASE}`));
  clearTimeout(settle.timer);
  settle.timer = setTimeout(() => {
    if (toward !== target) return;
    toward = null;
    target === 0 ? opened() : home();
  }, ms + 30);
}

function opened() {
  state = "open";
  write();
  pausePrompt();
  enterHistory();
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

function home() {
  state = "parked";
  y = innerHeight;
  Object.assign(frame.style, {
    transition: "none",
    transform: "translateY(100%)",
    pointerEvents: "none",
  });
  const s = stage();
  if (s) Object.assign(s.style, { transition: "none", transform: "", willChange: "" });
  resumePrompt();
  post({ type: "mime:sheet", open: false }); // Park the guest on its slot.
  leaveHistory();
  window.focus();
}

// Release: past a sixth of the screen or a flick finishes, else spring back.
// `travel` is how far toward the other end the input went; `speed` > 0 means
// heading there.
function release(from, travel, speed) {
  const done = travel > innerHeight * COMMIT || speed > FLICK;
  if (from === "home") settle(done ? 0 : innerHeight);
  else settle(done ? innerHeight : 0);
}

// ✋ Down: a curtain swipe, handed over by the prompt piece once it engages.
function grab(startY) {
  if (state !== "parked" && state !== "cold") return;
  begin();
  const y0 = lastDownY ?? startY;
  const samples = [[performance.now(), y0]];
  let last = y0;
  const move = (e) => {
    last = e.clientY;
    samples.push([performance.now(), last]);
    if (samples.length > 6) samples.shift();
    follow(innerHeight - (y0 - last));
  };
  const end = () => {
    removeEventListener("pointermove", move, true);
    removeEventListener("pointerup", end, true);
    removeEventListener("pointercancel", end, true);
    const [t0, a] = samples[0];
    const [t1, b] = samples.at(-1);
    release("home", y0 - last, t1 > t0 ? (a - b) / (t1 - t0) : 0);
  };
  addEventListener("pointermove", move, { capture: true, passive: true });
  addEventListener("pointerup", end, { capture: true, passive: true });
  addEventListener("pointercancel", end, { capture: true, passive: true });
}

// 🖱️ Wheels, either way. A trackpad streams small deltas, momentum included,
// and settles when they stop; a mouse notch is one big step and goes at once.
// `dy` > 0 always means "toward the other end".
let wheel = null;

function wheelStep(from, dy, mode) {
  lastWheelAt = performance.now();
  const px = mode === 1 ? dy * 16 : mode === 2 ? dy * innerHeight : dy;
  if (!wheel) {
    if (px <= 0) return;
    if (mode !== 0 || px >= 60) return settle(from === "home" ? 0 : innerHeight);
    begin();
    wheel = { from, travel: 0, at: lastWheelAt, speed: 0, timer: 0 };
  }
  wheel.speed = px / Math.max(1, lastWheelAt - wheel.at);
  wheel.at = lastWheelAt;
  wheel.travel = Math.max(0, wheel.travel + px);
  follow(wheel.from === "home" ? innerHeight - wheel.travel : wheel.travel);
  clearTimeout(wheel.timer);
  if (wheel.travel >= innerHeight * 0.5) return endWheel(true);
  wheel.timer = setTimeout(() => endWheel(), 140);
}

function endWheel(force) {
  if (!wheel) return;
  clearTimeout(wheel.timer);
  const { from, travel, speed } = wheel;
  wheel = null;
  release(from, force ? innerHeight : travel, speed);
}

addEventListener("wheel", (e) => {
  lastWheelAt = performance.now();
  if (!frame || !armed || e.ctrlKey) return; // ctrl+wheel is pinch-zoom
  if (Math.abs(e.deltaX) > Math.abs(e.deltaY)) return;
  if (wheel?.from === "home" || (!wheel && state === "parked"))
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
      if (pull) release("open", y, d.velocity || 0);
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
