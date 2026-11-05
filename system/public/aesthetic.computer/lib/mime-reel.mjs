// 🎞️ Mime Reel
// One AC runtime with live, silent object neighbors. Prepared object instances
// paint while sliding into view and become the active piece without restarting.
// Other pieces retain their captured-frame / poster fallback.
//
// It only runs in a noauth MIME runtime (`?noauth=true&mime=1&reel=1`), so a
// stranger's piece never sees the viewer's session. The prompt's mime sheet
// hosts this runtime in a single frame and owns the trip back to the prompt.
//
//   idle → drag → settle → (waiting → jumping → loading → reveal) → idle
//
// Input belongs to the piece except for a vertical drag. A press is held back
// until it shows what it is: a mostly-vertical move moves the reel, any other
// move, a lift or a still hold (HOLD ms) hands the held events to the piece in
// order. So taps, sideways drags and press-and-hold drawing all reach the card.

import { objectPiece as makeObject, isObjectLisp } from "./object/viewer.mjs?v=live-4";
import { qrcode as qr, ErrorCorrectLevel } from "../dep/@akamfoad/qr/qr.mjs";

const objects = new Map(); // address → prepared module; bounded to nearby cards
const hudFrames = new Map();

// Disk calls this at activation, reusing the exact instance painted in the swipe.
export function objectPiece(source, address) {
  let entry = objects.get(address);
  if (!entry || entry.source !== source) {
    entry = { source, piece: makeObject(source, address) };
    objects.set(address, entry);
  }
  return entry.piece;
}

// Save complete native overlays for revisits and for the outgoing live frame.
export function observe(overlays, screen) {
  if (phase !== "idle" || !cards[index] || !objects.has(cards[index].address)) return;
  hudFrames.set(cards[index].key, {
    w: screen.width, h: screen.height,
    layers: Object.values(overlays).filter(o => o?.img?.pixels)
      .map(o => ({ ...o, img: { ...o.img, pixels: new Uint8ClampedArray(o.img.pixels) } })),
  });
}

const MEDIA = [23, 21, 38, 255]; // mime's dark media colour
const SLOP = 10; // px of movement before a press says what it is
const HOLD = 180; // ms a still press waits before it belongs to the piece
const COMMIT = 0.5; // of the screen: past halfway the trip finishes
const FLICK = 0.0009; // screens per ms that also finish a trip...
const FLICK_MIN = 0.06; // ...once it has gone at least this far
const REVEAL = 180; // ms to blend a preview into its first live frames
const GLIDE = 35; // ms time constant of a settle
const WHEEL_REST = 140; // ms of wheel silence that ends a stream
const LOAD_TIMEOUT = 10000; // a card that never paints still gets revealed
const FRAMES_KEPT = 6;

let send, jump, needsPaint, preload;
let hosted = false; // Inside the prompt's mime sheet.
let open = true; // Hosted reels start parked until the sheet opens.

const cards = [];
const seen = new Set();
let page = 0;
let hasMore = true;
let fetching = null;
const seed = Math.floor(Math.random() * 1e9);

let index = -1; // The card on screen; -1 before the first one lands.
let phase = "idle";
let arrival = null; // Loading runs independently of a live card's next gesture.
let trip = 0; // Invalidates jumps queued behind a previous piece load.
let composed = false; // The displayed live preview already contains its HUD.
let offset = 0; // px; > 0 shows the next card coming up from below.
let goal = 0;
let toward = 0; // A settle's direction: 1 next, -1 previous, 0 back home.
let lastStep = 0;
let current = null; // { w, h, pixels }: the image standing in for this card.
let loadSince = 0;
let revealSince = 0;
let neighbors = null; // Fixed for one gesture, including its loading handoff.
let restore = null; // Clean piece pixels to resume after a cancelled swipe.

let frozen = false; // The next frame is captured as `current` before drawing.
let firstCard = false; // The first card's reveal tells the host we're ready.
let lastH = 0; // Screen height at the last drawn frame.

let gesture = null; // { mode: "pending" | "reel" | "piece" | "eat", ... }
let wheel = null; // { travel, timer }
let wheelCommitted = false;
let lastWheelAt = -Infinity;
let pulling = false; // A downward drag on the first card went to the host.

const frames = new Map(); // card key → { w, h, pixels }, the last frame seen
const posters = new Map(); // card key → ImageBitmap | null (failed)
const warmed = new Set();
const fitted = new Map(); // card key + size → pixels
let blank = null; // { w, h, pixels } in MEDIA

// 🚪 Called by disk once, when the runtime boots in reel mode.
export function start(options) {
  ({ send, jump, needsPaint, preload } = options);
  hosted = options.hosted === true;
  open = !hosted;
  if (hosted) post({ type: "mime:sheet:hello" });
  more().then(() => {
    if (cards.length) go(0, true);
    else post({ type: "mime:sheet:ready" }); // Nothing to show, but not stuck.
  });
}

export function setOpen(on) {
  open = on === true;
  if (!open) {
    gesture = null;
    if (wheel) endWheel(true);
    else if (phase === "drag") settle(0);
  } else if (index >= 0) at();
}

function post(message) {
  send({ type: "mime:reel", content: message });
}

function at() {
  const card = cards[index];
  if (card && open) post({ type: "mime:sheet:at", permalink: card.permalink, address: card.address });
}

// 📰 The feed, a page at a time. Only cards that run as AC pieces.
function more() {
  if (fetching || !hasMore) return fetching || Promise.resolve();
  fetching = fetch("/api/mime?feed=1&page=" + page + "&seed=" + seed)
    .then((r) => (r.ok ? r.json() : Promise.reject(new Error(r.status))))
    .then((data) => {
      page += 1;
      hasMore = data.hasMore === true;
      for (const post of data.recent || []) {
        const card = toCard(post);
        if (!card || seen.has(card.key)) continue;
        seen.add(card.key);
        cards.push(card);
      }
      if (index >= 0) warm();
    })
    .catch((err) => {
      console.warn("🎞️ Reel: feed unavailable", err);
      hasMore = false;
    })
    .finally(() => (fetching = null));
  return fetching;
}

const ADDRESS =
  /^(\$?[A-Za-z0-9_-]{1,64}|@[A-Za-z0-9_.-]{1,64}\/[A-Za-z0-9_-]{1,100})$/;

function toCard(post) {
  const media = post?.media;
  if (!media || typeof media.code !== "string") return null;
  let address, poster = null, label;
  if (media.kind === "kidlisp") {
    address = "$" + media.code;
    poster = "https://oven.aesthetic.computer/grab/webp/512/512/" +
      encodeURIComponent(address);
    label = "$" + media.code;
  } else if (media.kind === "piece") {
    // A registered piece runs at its handle address, which rides in the URL.
    address = media.code;
    try {
      const home = new URL(media.url || "", "https://aesthetic.computer");
      if (home.origin === "https://aesthetic.computer" && home.pathname.length > 1)
        address = decodeURIComponent(home.pathname.slice(1));
    } catch {}
    label = address;
  } else return null; // Video, docs, packs and replays stay on mime.ac.
  if (!ADDRESS.test(address)) return null;
  const title = media.title && media.title !== label ? label + " · " + media.title : label;
  return {
    key: post.code,
    permalink: post.code,
    address,
    poster,
    caption: title + (post.name ? "  " + post.name : ""),
  };
}

// 🧭 Put card `i` on screen.
function go(i, first = false, preview = null) {
  const card = cards[i];
  if (!card) return;
  index = i;
  offset = goal = 0;
  toward = 0;
  phase = "waiting"; // Until disk can take the jump (a load may be in flight).
  arrival = "waiting";
  const ticket = ++trip;
  loadSince = performance.now();
  current = preview || imageFor(card, current?.w, current?.h) || current;
  neighbors = null;
  warm();
  if (cards.length - i < 4) more();
  at();
  jump(card.address, () => {
    if (ticket !== trip) return false;
    arrival = "jumping";
    if (phase === "waiting") phase = "jumping";
    return true;
  });
  needsPaint();
  if (first) firstCard = true;
}

// Disk calls this when a piece load swaps in its module.
export function loadStarted() {
  if (arrival !== "jumping") return;
  arrival = "loading";
  if (phase === "jumping") phase = "loading";
}

function reveal(screen) {
  if (current && screen?.pixels) {
    // Keep the exact displayed preview, including its AC caption.
    current = { w: screen.width, h: screen.height, pixels: new Uint8ClampedArray(screen.pixels) };
    phase = "reveal";
    revealSince = performance.now();
  } else {
    phase = "idle";
    current = null;
  }
  if (firstCard) {
    firstCard = false;
    post({ type: "mime:sheet:ready" });
  }
  needsPaint();
}

// 🖼️ Images that stand in for a card.
function remember(card, screen) {
  if (!card || !screen?.pixels) return;
  frames.delete(card.key);
  frames.set(card.key, {
    w: screen.width,
    h: screen.height,
    pixels: new Uint8ClampedArray(screen.pixels),
  });
  while (frames.size > FRAMES_KEPT) frames.delete(frames.keys().next().value);
}

function imageFor(card, w, h) {
  if (!card || !w || !h) return null;
  const frame = frames.get(card.key);
  if (frame && frame.w === w && frame.h === h) return frame;
  const bitmap = posters.get(card.key);
  if (bitmap) {
    const id = card.key + ":" + w + "x" + h;
    let pixels = fitted.get(id);
    if (!pixels) {
      pixels = cover(bitmap, w, h);
      if (pixels) {
        fitted.set(id, pixels);
        while (fitted.size > FRAMES_KEPT * 2) fitted.delete(fitted.keys().next().value);
      }
    }
    if (pixels) return { w, h, pixels, poster: true };
  }
  return null;
}

function cover(bitmap, w, h) {
  if (typeof OffscreenCanvas === "undefined") return null;
  try {
    const canvas = new OffscreenCanvas(w, h);
    const ctx = canvas.getContext("2d");
    const s = Math.max(w / bitmap.width, h / bitmap.height);
    const dw = bitmap.width * s, dh = bitmap.height * s;
    ctx.imageSmoothingEnabled = false;
    ctx.drawImage(bitmap, (w - dw) / 2, (h - dh) / 2, dw, dh);
    return ctx.getImageData(0, 0, w, h).data;
  } catch {
    return null;
  }
}

function warm() {
  // Fill disk's source cache without booting extra runtimes.
  for (const i of [index + 1, index + 2, index + 3, index + 4]) {
    const card = cards[i];
    if (!preload || !card || (warmed.has(card.key) && (objects.has(card.address) || !card.object))) continue;
    warmed.add(card.key);
    Promise.resolve().then(() => preload(card.address)).then(source => {
      if (isObjectLisp(source) && cards.slice(Math.max(0, index - 1), index + 5).includes(card)) {
        card.object = true;
        objectPiece(source, card.address);
      }
    }).catch(() => warmed.delete(card.key));
  }
  const nearby = new Set(cards.slice(Math.max(0, index - 2), index + 6).map(card => card.key));
  const addresses = new Set(cards.slice(Math.max(0, index - 2), index + 6).map(card => card.address));
  for (const address of objects.keys()) if (!addresses.has(address)) objects.delete(address);
  for (const key of hudFrames.keys()) if (!nearby.has(key)) hudFrames.delete(key);
  for (const [key, bitmap] of posters) if (!nearby.has(key)) {
    bitmap?.close();
    posters.delete(key);
  }
  for (const i of [index, index - 1, index + 1, index + 2]) {
    const card = cards[i];
    if (!card?.poster || posters.has(card.key)) continue;
    posters.set(card.key, null);
    if (typeof createImageBitmap !== "function") continue;
    fetch(card.poster)
      .then((r) => (r.ok ? r.blob() : Promise.reject()))
      .then((blob) => createImageBitmap(blob))
      .then((bitmap) => {
        if (!posters.has(card.key)) return bitmap.close();
        posters.set(card.key, bitmap);
        if (phase !== "idle") needsPaint();
      })
      .catch(() => {});
  }
}

function blankFor(w, h) {
  if (blank?.w === w && blank?.h === h) return blank;
  const pixels = new Uint8ClampedArray(w * h * 4);
  for (let i = 0; i < pixels.length; i += 4) pixels.set(MEDIA, i);
  return (blank = { w, h, pixels, blank: true });
}

// ✋ Pens. Return true when the reel keeps the event (it may hand it on later).
export function pen(data, dispatch) {
  if (!open || index < 0) return false;
  const name = data.name;
  if (name !== "touch" && name !== "draw" && name !== "lift") return false;
  const now = performance.now();
  if (!data.isPrimary) {
    // A second finger means the piece: hand over a press still deciding.
    if (gesture?.mode === "pending") return handOver(dispatch), false;
    return gesture?.mode === "reel" || gesture?.mode === "eat";
  }

  if (name === "touch") {
    if (phase === "settle" && toward && neighbors?.has(toward))
      go(index + toward, false, neighbors.get(toward));
    if (phase === "drag" || phase === "settle") {
      // Catch the reel mid-glide.
      gesture = { mode: "reel", y0: data.y + offset, samples: [[now, data.y]] };
      phase = "drag";
      toward = 0;
      return true;
    }
    if (phase !== "idle" && !canSwipePreview()) {
      gesture = { mode: "eat" }; // A static preview cannot accept piece input yet.
      return true;
    }
    gesture = { mode: "pending", x0: data.x, y0: data.y, t0: now, held: [data] };
    return true;
  }

  if (!gesture) return false;

  if (gesture.mode === "piece" || gesture.mode === "eat") {
    const eat = gesture.mode === "eat";
    if (name === "lift") gesture = null;
    return eat;
  }

  if (gesture.mode === "pending") {
    gesture.held.push(data);
    const dx = data.x - gesture.x0, dy = data.y - gesture.y0;
    if (name === "lift") return handOver(dispatch, true);
    if (Math.abs(dy) > SLOP && Math.abs(dy) > Math.abs(dx) * 1.4) {
      engage(data, now, dispatch);
      return true;
    }
    if (Math.hypot(dx, dy) > SLOP) return handOver(dispatch);
    return true;
  }

  // mode === "reel"
  if (name === "draw") {
    gesture.samples.push([now, data.y]);
    while (gesture.samples.length > 2 && now - gesture.samples[0][0] > 100)
      gesture.samples.shift();
    if (gesture.pull) return true; // The host follows this one itself.
    follow(gesture.y0 - data.y);
    return true;
  }
  if (name === "lift") {
    const g = gesture;
    gesture = null;
    if (g.pull) return true; // Bios ends the pull from its own pointerup.
    const [t0, y0] = g.samples[0], [t1, y1] = g.samples.at(-1);
    const v = now - t1 <= 100 && t1 > t0 ? (y0 - y1) / (t1 - t0) : 0;
    release(v);
    return true;
  }
  return true;
}

function handOver(dispatch, ended = false) {
  if (arrival) {
    gesture = ended ? null : { mode: "eat" };
    return true; // Never deliver taps to the outgoing piece while loading.
  }
  const held = gesture.held;
  gesture = ended ? null : { mode: "piece" };
  for (const e of held) dispatch(e);
  return true;
}

function engage(data, now, dispatch) {
  const g = gesture;
  const downward = data.y > g.y0; // Pulling the previous card down.
  if (downward && index === 0 && hosted) {
    gesture = { mode: "reel", pull: true, samples: [[now, data.y]] };
    post({ type: "mime:sheet:pull", start: true });
    return;
  }
  gesture = { mode: "reel", y0: g.y0, samples: [[g.t0, g.y0], [now, data.y]] };
  composed = phase !== "idle";
  phase = "drag";
  toward = 0;
  frozen = true;
  follow(g.y0 - data.y);
}

// Where the reel wants to be, bounded by the cards that exist.
function follow(next) {
  const h = lastH || 1;
  let o = next;
  if (o < 0 && index === 0) o *= 0.3; // Nothing above: rubber band.
  if (o > 0 && index === cards.length - 1) o *= 0.3;
  goal = Math.max(-h, Math.min(h, o));
  offset = goal; // A finger is followed exactly.
  needsPaint();
}

function release(v = 0) {
  const h = lastH || 1;
  const frac = offset / h;
  let dir = 0;
  if (Math.abs(frac) >= COMMIT) dir = Math.sign(frac);
  else if (Math.abs(v / h) > FLICK && Math.abs(frac) > FLICK_MIN && Math.sign(v) === Math.sign(frac))
    dir = Math.sign(frac);
  if (dir > 0 && !cards[index + 1]) dir = 0;
  if (dir < 0 && !cards[index - 1]) dir = 0;
  settle(dir);
}

function settle(dir) {
  toward = dir;
  goal = dir * (lastH || 1);
  phase = "settle";
  lastStep = performance.now();
  needsPaint();
}

// 🖱️ Wheels move the reel; a stream ends after a short silence.
export function scroll(content) {
  if (!open || index < 0) return false;
  const dy = content?.y || 0, dx = content?.x || 0;
  if (Math.abs(dx) > Math.abs(dy) || !dy) return false;
  const now = performance.now();
  if (now - lastWheelAt > WHEEL_REST) wheelCommitted = false;
  lastWheelAt = now;
  // One trackpad stream commits one card; momentum must not grab the settle
  // again or spill into the following card after loading completes.
  if (wheelCommitted) return true;
  if ((phase === "waiting" || phase === "jumping" || phase === "loading") && !canSwipePreview()) return true;
  if (!wheel) {
    if (dy < 0 && index === 0) {
      if (hosted) post({ type: "mime:sheet:wheel", dy: -dy });
      return true;
    }
    wheel = { travel: phase === "settle" ? offset : 0, timer: 0 };
    if (phase !== "drag" && phase !== "settle") {
      composed = phase !== "idle";
      frozen = true;
    }
    phase = "drag";
    toward = 0;
  }
  wheel.travel += dy;
  follow(wheel.travel);
  clearTimeout(wheel.timer);
  if (Math.abs(offset) >= (lastH || 1) * COMMIT) {
    wheelCommitted = true;
    endWheel();
    return true;
  }
  wheel.timer = setTimeout(() => endWheel(), WHEEL_REST);
  return true;
}

function endWheel(quiet = false) {
  if (!wheel) return;
  clearTimeout(wheel.timer);
  wheel = null;
  if (quiet) return settle(0);
  release(0);
  needsPaint();
}

// ⌨️ Page Up / Page Down step a card; every other key belongs to the piece.
export function key(data) {
  if (!open || index < 0 || !data.name?.startsWith("keyboard:down")) return false;
  const k = data.key;
  if (k !== "PageDown" && k !== "PageUp") return false;
  if (phase !== "idle" && !canSwipePreview()) return true;
  const dir = k === "PageDown" ? 1 : -1;
  if (dir < 0 && index === 0) {
    if (hosted) post({ type: "mime:sheet:back" });
    return true;
  }
  if (!cards[index + dir]) return true;
  composed = phase !== "idle";
  frozen = true;
  phase = "settle";
  toward = dir;
  goal = dir * (lastH || 1);
  lastStep = performance.now();
  needsPaint();
  return true;
}

// 🎨 Per frame. `hold` says whether the piece's own paint should be skipped
// and the reel drawn instead; disk calls it before painting.
function canSwipePreview() {
  return current !== null; // A cold card can keep sliding while its source arrives.
}

export function busy() {
  return phase !== "idle" || frozen;
}

export function hold(pieceReady, screen) {
  if (restore && screen?.pixels) {
    if (restore.w === screen.width && restore.h === screen.height) screen.pixels.set(restore.pixels);
    restore = null;
  }
  if (gesture?.mode === "pending" && performance.now() - gesture.t0 > HOLD)
    gesture.timedOut = true; // Handed over by `tick` before the next pens.
  if (arrival === "loading" && pieceReady) {
    arrival = null;
    if (phase === "loading") {
      reveal(screen);
      return false;
    }
  }
  if ((phase === "waiting" || phase === "jumping" || phase === "loading") &&
      performance.now() - loadSince > LOAD_TIMEOUT) {
    arrival = null;
    reveal(screen);
    return false;
  }
  return (phase !== "idle" && phase !== "reveal") || frozen;
}

// Disk calls this at the top of each frame's pens, to hand over a press that
// has been held still long enough.
export function tick(dispatch) {
  if (gesture?.mode === "pending" &&
      (gesture.timedOut || performance.now() - gesture.t0 > HOLD))
    handOver(dispatch);
}

export function draw($, overlays = {}) {
  const { screen } = $;
  const w = screen.width, h = screen.height, px = screen.pixels;
  if (!px || !w || !h) return;
  lastH = h;

  if (phase === "reveal") {
    // Let the piece paint normally, then blend complete AC frames so its HUD
    // appears with the artwork. Restore its clean backing before the next paint.
    restore = { w, h, pixels: new Uint8ClampedArray(px) };
    const live = { w, h, pixels: px };
    for (const overlay of Object.values(overlays)) composite(live, overlay);
    const t = Math.min(1, (performance.now() - revealSince) / REVEAL);
    const amount = t * t * (3 - 2 * t);
    if (sized(current, w, h)) {
      for (let i = 0; i < px.length; i++) px[i] = current.pixels[i] * (1 - amount) + px[i] * amount;
    }
    consume(overlays);
    if (t === 1) { phase = "idle"; current = null; }
    needsPaint();
    return;
  }

  if (frozen) {
    frozen = false;
    const backing = composed ? null : { w, h, pixels: new Uint8ClampedArray(px) };
    current = { w, h, pixels: new Uint8ClampedArray(px), backing };
    // BIOS normally paints these after the piece. Freeze them into the same
    // screen before moving it, then suppress the stationary copies below.
    if (!composed) for (const overlay of Object.values(overlays)) composite(current, overlay);
    composed = false;
    remember(cards[index], { width: w, height: h, pixels: current.pixels });
    neighbors = new Map([-1, 1].map(dir => [dir,
      liveFor($, cards[index + dir], w, h) || imageFor(cards[index + dir], w, h) || blankFor(w, h)]));
    warm();
  }

  if (phase === "settle") {
    const now = performance.now();
    const dt = Math.min(64, now - lastStep);
    lastStep = now;
    goal = toward * h;
    const gap = goal - offset;
    offset = Math.abs(gap) < 1 ? goal : offset + gap * (1 - Math.exp(-dt / GLIDE));
  }

  // Only neighbors ready when the gesture began can turn live: a late fetch
  // must not replace a poster halfway through a swipe.
  if (current?.live || ((phase === "drag" || phase === "settle") && objects.has(cards[index]?.address))) {
    const live = liveFor($, cards[index], w, h);
    if (live) current = { ...live, backing: current?.backing };
  }
  for (const dir of [-1, 1]) if (neighbors?.get(dir)?.live && Math.sign(offset) === dir) {
    const live = liveFor($, cards[index + dir], w, h);
    if (live) neighbors.set(dir, live);
  }
  const o = Math.round(offset);
  const here = sized(current, w, h) || imageFor(cards[index], w, h) || blankFor(w, h);
  blit(px, w, h, here, -o);
  const next = sized(neighbors?.get(1), w, h) || blankFor(w, h);
  const previous = sized(neighbors?.get(-1), w, h) || blankFor(w, h);
  if (o > 0) blit(px, w, h, next, h - o);
  if (o < 0) blit(px, w, h, previous, -h - o);

  caption($, cards[index], -o, h, here);
  if (o > 0) caption($, cards[index + 1], h - o, h, next);
  if (o < 0) caption($, cards[index - 1], -h - o, h, previous);
  // The translated image already owns these layers, even on the frame that
  // settles or starts loading. BIOS must not paint another HUD over it.
  consume(overlays);

  if (phase === "settle" && offset === goal) {
    if (toward === 0) {
      phase = arrival || "idle";
      restore = current?.backing || null;
      if (!arrival) current = null;
      neighbors = null;
      needsPaint();
    } else {
      const next = index + toward;
      const preview = sized(neighbors?.get(toward), w, h) || blankFor(w, h);
      go(next, false, preview);
    }
  } else if (phase !== "idle") needsPaint();
}

function sized(image, w, h) {
  return image && image.w === w && image.h === h ? image : null;
}

// Copy a full-width image into the screen with its top edge at `top`.
function blit(px, w, h, image, top) {
  const y0 = Math.max(0, top), y1 = Math.min(h, top + h);
  if (y1 <= y0) return;
  const row = w * 4;
  px.set(image.pixels.subarray((y0 - top) * row, (y1 - top) * row), y0 * row);
}

function caption($, card, top, h, image) {
  if (!card || top >= h || top + h <= 0) return;
  if (image && !image.blank) return; // Captured HUD travels with its frame; posters need no extra label.
  // Anchor to the card, never clamp its label to the viewport edge.
  const y = top + h - 18;
  if (y < 0 || y >= h) return;
  $.ink(0, 0, 0, 160).write(card.caption, { x: 7, y: y + 1 });
  $.ink(255, 255, 255).write(card.caption, { x: 6, y });
}

function consume(overlays) {
  for (const [name, overlay] of Object.entries(overlays)) {
    if (overlay?.img?.pixels) delete overlays[name];
  }
}

// Composite a raster HUD layer at its normal AC coordinates. Do not mutate
// its source buffer: the runtime may cache it for later frames.
function composite(frame, overlay) {
  const image = overlay?.img;
  if (!image?.pixels || !Number.isFinite(overlay.x) || !Number.isFinite(overlay.y)) return;
  const x = Math.round(overlay.x), y = Math.round(overlay.y);
  const alpha = Math.max(0, Math.min(1, overlay.opacity ?? 1));
  for (let sy = Math.max(0, -y); sy < Math.min(image.height, frame.h - y); sy++) {
    for (let sx = Math.max(0, -x); sx < Math.min(image.width, frame.w - x); sx++) {
      const src = (sy * image.width + sx) * 4, dst = ((sy + y) * frame.w + sx + x) * 4;
      const a = image.pixels[src + 3] / 255 * alpha;
      if (!a) continue;
      const b = frame.pixels[dst + 3] / 255 * (1 - a), out = a + b;
      for (let c = 0; c < 3; c++) frame.pixels[dst + c] = (image.pixels[src + c] * a + frame.pixels[dst + c] * b) / out;
      frame.pixels[dst + 3] = out * 255;
    }
  }
}

// Render only visible model neighbors. The same renderer, camera and animation
// clock are used by the active piece; no second runtime or audio is booted.
function liveFor($, card, w, h) {
  const entry = objects.get(card?.address);
  if (!entry || !$.painting) return null;
  const rendered = $.painting(w, h, api => {
    entry.piece.paint(api);
    const saved = hudFrames.get(card.key);
    if (saved?.w === w && saved.h === h) {
      const frame = { w, h, pixels: api.screen.pixels };
      for (const overlay of saved.layers) composite(frame, overlay);
    } else {
      // The active corner label uses the default font_1, already loaded by disk.
      api.ink(230, 232, 240).write(card.address, { x: 6, y: 10 }, undefined, undefined, false);
      const cells = entry.qr ||= qr("https://prompt.ac/" + card.address, { errorCorrectLevel: ErrorCorrectLevel.M }).modules;
      const x0 = w - cells.length - 5, y0 = h - cells.length - 6;
      for (let y = 0; y < cells.length; y++) for (let x = 0; x < cells.length; x++)
        api.ink(cells[y][x] ? "black" : "white").box(x0 + x, y0 + y, 1, 1);
      const tw = card.address.length * 4;
      api.ink("black").box(w - tw - 6, y0 - 9, tw + 2, 9);
      api.ink("white").write(card.address, { x: w - tw - 5, y: y0 - 9 }, undefined, undefined, false, "MatrixChunky8");
    }
  });
  return rendered?.pixels ? { w, h, pixels: rendered.pixels, live: true } : null;
}
