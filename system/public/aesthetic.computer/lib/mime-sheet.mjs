// 🎭 Mime Sheet
// The mime feed as the screen below the prompt. This page is the host: it
// keeps a same-origin frame of /mime/?embed=prompt (the guest) parked under
// the canvas, slides it up with the curtain swipe, and hands back when the
// guest scrolls into the transparent slot it keeps above its first card.
//
//   cold → warming → ready            (loaded on idle, parked on the slot)
//   grab → dragging → presenting → open → returning → parked
//
// The prompt piece only says where a swipe began (`grab`) or asks to `show`;
// the finger is followed here on the main thread, a write per frame. While
// the sheet covers the screen the prompt's loop pauses, and it resumes as soon
// as the guest starts scrolling back toward it. The address bar follows the
// card on screen (/mime/?p=<code>) and Back closes the sheet.

const MEDIA = "#171526"; // mime's dark media colour, behind a guest still loading
const ease = "cubic-bezier(.2,.8,.2,1)";

let frame;
let state = "cold"; // cold | parked | dragging | presenting | open | returning
let pausedLoop = false;
let lastDownY = null; // clientY of the latest pointerdown, for an exact grab.
let historyEntry = false; // We pushed a /mime/?p= entry for the open sheet.
let swallowPop = false;
let lastPermalink = null; // The guest's card on screen, as last announced.

const post = (message) =>
  frame?.contentWindow?.postMessage(message, location.origin);

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
    visibility: "hidden",
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

// 🔁 Pausing the prompt behind the sheet.
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
function enterHistory(permalink = lastPermalink) {
  const url = "/mime/" + (permalink ? "?p=" + encodeURIComponent(permalink) : "");
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
  if (state === "open" || state === "presenting") {
    historyEntry = false;
    dismiss();
    return true;
  }
  if (e?.state?.acMimeSheet) {
    // Forward into a sheet entry: open it again.
    historyEntry = true;
    show();
    return true;
  }
  return false;
}

// ✋ Following the finger.
let drag = null;
let raf = 0;

function place(y) {
  frame.style.transform = `translateY(${Math.max(0, y)}px)`;
}

function grab(startY) {
  const f = ensure();
  if (state === "open" || state === "presenting") return;
  state = "dragging";
  post({ type: "mime:sheet", open: true }); // Bring the first card up.
  f.style.transition = "none";
  f.style.willChange = "transform";
  f.style.visibility = "visible";
  const y0 = lastDownY ?? startY;
  drag = { y0, y: y0, samples: [[performance.now(), y0]] };
  const move = (e) => {
    if (!drag) return;
    drag.y = e.clientY;
    drag.samples.push([performance.now(), e.clientY]);
    if (drag.samples.length > 6) drag.samples.shift();
    if (!raf)
      raf = requestAnimationFrame(() => {
        raf = 0;
        if (drag) place(innerHeight - (drag.y0 - drag.y));
      });
  };
  const end = () => {
    removeEventListener("pointermove", move, true);
    removeEventListener("pointerup", end, true);
    removeEventListener("pointercancel", end, true);
    if (!drag) return;
    const pulled = drag.y0 - drag.y;
    const [t0, ya] = drag.samples[0];
    const [t1, yb] = drag.samples.at(-1);
    const velocity = t1 > t0 ? (ya - yb) / (t1 - t0) : 0; // px/ms, upward +
    drag = null;
    if (pulled > innerHeight * 0.15 || velocity > 0.45) show();
    else cancel();
  };
  addEventListener("pointermove", move, { capture: true, passive: true });
  addEventListener("pointerup", end, { capture: true, passive: true });
  addEventListener("pointercancel", end, { capture: true, passive: true });
}

function slide(to, done) {
  const f = frame;
  const from = new DOMMatrix(getComputedStyle(f).transform).m42;
  const target = to === "up" ? 0 : innerHeight;
  const ms = Math.round(140 + 260 * Math.min(1, Math.abs(target - from) / innerHeight));
  f.style.willChange = "transform";
  f.style.transition = `transform ${ms}ms ${ease}`;
  requestAnimationFrame(() => (f.style.transform = `translateY(${target}px)`));
  let finished = false;
  const finish = () => {
    if (finished) return;
    finished = true;
    f.style.willChange = "";
    done?.();
  };
  f.addEventListener("transitionend", finish, { once: true });
  setTimeout(finish, ms + 80); // transitionend can go missing.
}

function show() {
  const f = ensure();
  if (state === "open" || state === "presenting") return;
  if (state !== "dragging") post({ type: "mime:sheet", open: true });
  state = "presenting";
  f.style.visibility = "visible";
  f.style.pointerEvents = "auto";
  enterHistory();
  slide("up", () => {
    if (state !== "presenting") return;
    state = "open";
    pausePrompt();
    post({ type: "mime:sheet:presented" });
    f.contentWindow?.focus();
  });
}

function cancel() {
  if (!frame) return;
  state = "parked";
  slide("down", () => {
    if (state !== "parked") return;
    frame.style.visibility = "hidden";
    post({ type: "mime:sheet", open: false });
  });
}

// Back while open: slide the whole sheet down, the way it came.
function dismiss() {
  resumePrompt();
  frame.style.pointerEvents = "none";
  state = "parked";
  slide("down", () => {
    if (state !== "parked") return;
    frame.style.visibility = "hidden";
    post({ type: "mime:sheet", open: false });
    window.focus();
  });
}

// The guest scrolled up into its slot: the prompt already shows through, so
// the frame drops away without a visible change.
function tuck() {
  resumePrompt();
  state = "parked";
  Object.assign(frame.style, {
    transition: "none",
    transform: "translateY(100%)",
    visibility: "hidden",
    pointerEvents: "none",
  });
  leaveHistory();
  window.focus();
}

addEventListener("message", (e) => {
  if (!frame || e.origin !== location.origin || e.source !== frame.contentWindow) return;
  const { type, permalink } = e.data || {};
  if (type === "mime:sheet:hello")
    post({ type: "mime:sheet", open: state === "dragging" || state === "presenting" || state === "open" });
  else if (type === "mime:sheet:ready") frame.style.background = "transparent";
  else if (type === "mime:sheet:at") {
    lastPermalink = permalink || null;
    if (historyEntry) enterHistory();
  } else if (type === "mime:sheet:leaving") {
    if (state === "open") state = "returning";
    resumePrompt(); // Live again before it comes into view.
  } else if (type === "mime:sheet:staying") {
    if (state !== "returning") return;
    state = "open";
    pausePrompt();
  } else if (type === "mime:sheet:closed") tuck();
});

window.acMimeSheet = { popstate };

// Messages from the prompt piece:
//   { action: "warm" }              load the guest on idle
//   { action: "grab", y, of }       a swipe began at y (piece px of `of` tall)
//   { action: "show" }              present without a drag (down arrow)
export function mimeSheet({ action, y, of } = {}) {
  if (action === "warm") warm();
  else if (action === "grab") grab(of ? (y * innerHeight) / of : innerHeight);
  else if (action === "show") show();
}
