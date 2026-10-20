// 🎭 Mime Sheet
// The mime feed as the screen below the prompt. A same-origin frame of
// /mime/?embed=prompt slides up over the canvas, following the prompt's swipe.
// Inside it, the feed keeps a transparent slot above its first card, so
// scrolling back up uncovers the live prompt and the frame tucks itself away.
// The frame stays loaded between visits, parked on that slot.

let frame;
let open = false;
let dragging = false;

const post = (message) =>
  frame?.contentWindow?.postMessage(message, location.origin);

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
    background: "transparent",
    colorScheme: "normal", // Matching schemes keep the frame see-through.
    transform: "translateY(100%)",
    visibility: "hidden",
    pointerEvents: "none",
  });
  document.body.append(frame);
  return frame;
}

window.addEventListener("message", (e) => {
  if (!frame || e.origin !== location.origin || e.source !== frame.contentWindow) return;
  if (e.data?.type === "mime:sheet:ready") post({ type: "mime:sheet", open: open || dragging });
  if (e.data?.type === "mime:sheet:closed") tuck();
});

function slide(f, to, ms, done) {
  f.style.transition = `transform ${ms}ms cubic-bezier(.2,.8,.2,1)`;
  requestAnimationFrame(() => (f.style.transform = to));
  if (done) {
    const end = () => done();
    f.addEventListener("transitionend", end, { once: true });
  }
}

// The finger is `dy` pixels up from where it went down.
function drag(dy) {
  const f = ensure();
  if (!dragging) {
    dragging = true;
    post({ type: "mime:sheet", open: true }); // Bring the first card up.
  }
  f.style.transition = "none";
  f.style.visibility = "visible";
  f.style.transform = `translateY(${Math.max(0, innerHeight - dy)}px)`;
}

function show() {
  const f = ensure();
  if (!dragging) post({ type: "mime:sheet", open: true });
  dragging = false;
  open = true;
  f.style.visibility = "visible";
  f.style.pointerEvents = "auto";
  slide(f, "translateY(0)", 320, () => f.contentWindow?.focus());
}

function cancel() {
  if (!frame) return;
  dragging = false;
  slide(frame, "translateY(100%)", 220, () => {
    if (open || dragging) return;
    frame.style.visibility = "hidden";
    post({ type: "mime:sheet", open: false });
  });
}

// The feed scrolled back up to its empty slot: the prompt is already showing
// through, so the frame can drop away without a visible change.
function tuck() {
  open = false;
  dragging = false;
  Object.assign(frame.style, {
    transition: "none",
    transform: "translateY(100%)",
    visibility: "hidden",
    pointerEvents: "none",
  });
  window.focus();
}

// Messages from the prompt piece: { action: "drag", dy, of } | "show" | "cancel".
// `dy` is in the piece's pixels, out of a screen `of` pixels tall.
export function mimeSheet({ action, dy = 0, of } = {}) {
  if (action === "drag") drag(of ? (dy * innerHeight) / of : dy);
  else if (action === "show") show();
  else if (action === "cancel") cancel();
}
