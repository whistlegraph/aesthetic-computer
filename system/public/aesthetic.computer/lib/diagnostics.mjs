// diagnostics.mjs — what the piece looks like from the inside, sent to whoever
// is authoring it.
//
// A piece runs in a worker. That is the whole reason this file exists: the
// worker has no `window`, so `window.addEventListener("error")` never fires for
// it, puppeteer's `pageerror` never fires for it, and a piece can therefore
// fail — or paint nothing at all — while every tool pointed at the page reports
// a clean bill of health. The author watching from a terminal sees a black
// rectangle and no reason for it.
//
// Two signals, because they catch different failures:
//
//   the canary — a cheap statistic about the frame that was actually handed to
//   the display. Not a judgement. A piece that deliberately wipes to one colour
//   is indistinguishable from a piece whose writes are being dropped, and this
//   module does not pretend to tell them apart; it reports "one colour" and
//   lets a human decide whether that is the piece working.
//
//   the log — console output and uncaught errors, forwarded with the level
//   intact.
//
// Both are rate-limited and both are silent until a channel is set. Nothing
// here reaches the network for a visitor who is merely looking at a piece.
// `channel(name)` is the switch, and it is only ever thrown for a session that
// is being authored.

// Pixels sampled per frame. Enough to find a stripe of colour in a field of
// black; few enough to cost nothing at 60fps.
export const FRAME_SAMPLE = 1024;
// A frame report at most this often, plus one whenever the signature changes.
// Without the heartbeat a piece that goes blank and stays blank would report
// once and then look like a dead connection.
export const HEARTBEAT_MS = 4000;
// Logs are bursty — a piece can put a line in the console every frame — so the
// budget is per second, and what it drops is counted rather than hidden.
export const MAX_LOGS_PER_SECOND = 20;
// How much of one log line survives. A serialized frame of pixel data in a
// console.log should not become a network event.
export const MAX_LOG_LENGTH = 2000;

// Quantize hard before counting: anti-aliasing and dithering would otherwise
// make every frame "thousands of colours" and the count would say nothing.
const QUANTIZE = 3; // bits dropped per channel

// Walking the frame at a fixed stride aliases against periodic content — a
// piece of stripes or a dither pattern lands on the same colour every time and
// reports itself blank, which is the one mistake this module must not make.
// Stepping by a prime that is coprime with the pixel count visits the frame in
// a full-period permutation instead, so the samples are spread across it
// whatever the content's period is.
const STEPS = [7919, 6151, 3079, 1543, 769, 389, 193, 97, 11, 3, 1];

function stepFor(total) {
  for (const step of STEPS) if (step < total && total % step !== 0) return step;
  return 1;
}

// A cheap description of one frame. `colors` is how many distinct quantized
// colours were sampled, `blank` means every sampled pixel was identical — which
// is the shape of both an honest monochrome and a piece whose writes went
// nowhere.
export function frameSignature(pixels, width, height, sample = FRAME_SAMPLE) {
  const count = Math.floor((Number(width) || 0) * (Number(height) || 0));
  if (!pixels || count <= 0) return null;
  // A detached buffer (the frame was transferred already) has no length.
  const length = pixels.length || 0;
  if (length < 4) return null;

  const total = Math.min(count, Math.floor(length / 4));
  const step = stepFor(total);
  const want = Math.min(total, sample);
  const seen = new Set();
  let first = -1;
  let sampled = 0;

  for (let n = 0; n < want; n += 1) {
    const o = ((n * step) % total) * 4;
    const key =
      ((pixels[o] >> QUANTIZE) << 16) |
      ((pixels[o + 1] >> QUANTIZE) << 8) |
      (pixels[o + 2] >> QUANTIZE);
    if (first < 0) first = o;
    seen.add(key);
    sampled += 1;
    // Once it is clearly not blank the exact count stops mattering, and a busy
    // frame should not cost a thousand Set insertions.
    if (seen.size > 64) break;
  }

  if (sampled === 0) return null;
  return {
    colors: seen.size,
    blank: seen.size === 1,
    sampled,
    // The colour it is stuck on, which is usually the `wipe` and is usually the
    // fastest way to recognise what happened.
    color:
      seen.size === 1 && first >= 0
        ? [pixels[first], pixels[first + 1], pixels[first + 2]]
        : null,
  };
}

export class Diagnostics {
  constructor({ send, now = () => Date.now(), heartbeat = HEARTBEAT_MS } = {}) {
    this.send = send;
    this.now = now;
    this.heartbeat = heartbeat;
    this.room = "";
    this.last = null; // The signature we last reported.
    this.lastAt = 0;
    this.window = 0; // The second the log budget is counted in.
    this.spent = 0;
    this.dropped = 0;
  }

  get on() {
    return Boolean(this.room);
  }

  // Point at a channel, or pass nothing to go quiet. Everything resets: the
  // counts described the piece that was being watched before.
  channel(name) {
    const next = String(name || "");
    if (next === this.room) return false;
    this.room = next;
    this.last = null;
    this.lastAt = 0;
    this.dropped = 0;
    return true;
  }

  #post(kind, body) {
    if (!this.on) return;
    try {
      this.send("diagnostics:report", { channel: this.room, kind, ...body });
    } catch {
      // A diagnostic that throws must not become the thing being diagnosed.
    }
  }

  // One painted frame, as it was handed to the display.
  frame(pixels, width, height) {
    if (!this.on) return;
    const signature = frameSignature(pixels, width, height);
    if (!signature) return;
    const at = this.now();
    const changed =
      !this.last ||
      this.last.blank !== signature.blank ||
      this.last.colors !== signature.colors;
    if (!changed && at - this.lastAt < this.heartbeat) return;
    this.last = signature;
    this.lastAt = at;
    this.#post("frame", {
      colors: signature.colors,
      blank: signature.blank,
      color: signature.color,
      width,
      height,
    });
  }

  // One console line or caught error. `args` arrives already serialized by the
  // caller, which owns the console hijack.
  note(level, args) {
    if (!this.on) return;
    const at = this.now();
    const second = Math.floor(at / 1000);
    if (second !== this.window) {
      // Report the burst that was dropped before forgetting it, so a flood is
      // visible as a flood rather than as silence.
      const missed = this.dropped;
      this.window = second;
      this.spent = 0;
      this.dropped = 0;
      if (missed > 0) this.#post("dropped", { count: missed });
    }
    this.spent += 1;
    if (this.spent > MAX_LOGS_PER_SECOND) {
      this.dropped += 1;
      return;
    }
    const text = (Array.isArray(args) ? args : [args])
      .map((value) => String(value ?? ""))
      .join(" ")
      .slice(0, MAX_LOG_LENGTH);
    if (!text) return;
    this.#post("log", { level: String(level || "log"), text, at });
  }

  // Uncaught errors, including the ones only the worker ever sees.
  watch(scope = globalThis) {
    if (!scope?.addEventListener) return this;
    scope.addEventListener("error", (event) => {
      const where = event?.filename
        ? ` at ${event.filename}:${event.lineno}:${event.colno}`
        : "";
      this.note("error", [`Uncaught: ${event?.message || event?.type}${where}`]);
    });
    scope.addEventListener("unhandledrejection", (event) => {
      this.note("error", [`Unhandled promise: ${event?.reason}`]);
    });
    return this;
  }
}
