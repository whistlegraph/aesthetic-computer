// The easel.
//
// The tool is named after the thing a painter stands a canvas on, so a session
// opens by standing one up. What goes on the canvas is the only two facts a
// session starts with that are worth knowing: the name of the piece, and the
// address where it can already be reached. Both are true from the first second
// now — the blank publishes as soon as the handle resolves — so the splash is
// reporting rather than promising.
//
// The name is written on rather than printed, one character at a time, because
// that is what an easel is for. The address follows once the name is finished.
// Nothing else moves: the frame and the legs are standing from the first frame,
// since an easel that assembles itself is a cartoon and this one has to sit
// above a transcript someone is about to read.
//
// Pure, like the mascot. A caller asks what the easel looks like at a given
// number of milliseconds and gets lines back. Nothing schedules, nothing draws,
// nothing reads a clock.

// Milliseconds per character of the piece name, and the pause before the
// address starts. Slow enough to read as handwriting, short enough that the
// whole thing is over before anyone has finished their first prompt.
const WRITE_MS = 55;
const PAUSE_MS = 260;
const ADDRESS_MS = 16;
// How long the finished easel stands before the caller may stop asking.
const HOLD_MS = 900;

const MIN_INNER = 13;
const MAX_INNER = 46;

function innerWidth(piece, address) {
  const longest = Math.max(piece.length, address.length);
  return Math.min(MAX_INNER, Math.max(MIN_INNER, longest + 2));
}

function centre(text, width) {
  if (text.length >= width) return text.slice(0, width);
  const left = Math.floor((width - text.length) / 2);
  return " ".repeat(left) + text + " ".repeat(width - text.length - left);
}

// The whole reveal, in milliseconds: name written, pause, address written, hold.
export function easelDuration(piece = "", address = "") {
  return (
    String(piece).length * WRITE_MS +
    PAUSE_MS +
    String(address).length * ADDRESS_MS +
    HOLD_MS
  );
}

// How many characters of each line are showing at `elapsed`.
function revealed(elapsed, piece, address) {
  const ms = Math.max(0, Number(elapsed) || 0);
  const written = Math.min(piece.length, Math.floor(ms / WRITE_MS));
  if (written < piece.length) return { piece: written, address: 0 };
  const after = ms - piece.length * WRITE_MS - PAUSE_MS;
  if (after <= 0) return { piece: piece.length, address: 0 };
  return {
    piece: piece.length,
    address: Math.min(address.length, Math.floor(after / ADDRESS_MS)),
  };
}

// One frame. `piece` is the slug as it appears on disk; `address` is the bare
// host+path the rock is showing. Either may be empty — a session that is not
// signed in has no address yet, and the canvas simply stays blank there rather
// than apologising for it.
export function easelFrame(elapsed = 0, { piece = "", address = "" } = {}) {
  const name = String(piece || "");
  const where = String(address || "");
  const inner = innerWidth(name, where);
  const shown = revealed(elapsed, name, where);

  // A cursor while the name is still being written, so the pause between the
  // name landing and the address starting does not read as a hang.
  const writing = shown.piece < name.length;
  const nameText = name.slice(0, shown.piece) + (writing ? "▌" : "");

  const top = "┌" + "─".repeat(inner) + "┐";
  const blank = "│" + " ".repeat(inner) + "│";
  const nameRow = "│" + centre(nameText, inner) + "│";
  const addressRow = "│" + centre(where.slice(0, shown.address), inner) + "│";

  // The bottom edge carries the mast the legs hang from, so the easel reads as
  // one object rather than a box resting on two sticks.
  const mastAt = Math.floor(inner / 2);
  const bottom =
    "└" + "─".repeat(mastAt) + "┬" + "─".repeat(inner - mastAt - 1) + "┘";

  const pad = " ".repeat(mastAt);
  return [
    top,
    blank,
    nameRow,
    blank,
    addressRow,
    bottom,
    pad + "╱ ╲",
    pad.slice(0, Math.max(0, mastAt - 1)) + "╱   ╲",
  ];
}

// When the easel next needs repainting, or null once it is finished standing
// there and the caller can stop asking.
export function easelNextFrame(elapsed = 0, { piece = "", address = "" } = {}) {
  const ms = Math.max(0, Number(elapsed) || 0);
  if (ms >= easelDuration(piece, address)) return null;
  const shown = revealed(ms, String(piece), String(address));
  if (shown.piece < String(piece).length) return WRITE_MS;
  if (shown.address < String(address).length) return ADDRESS_MS;
  return HOLD_MS;
}

export const EASEL_HEIGHT = 8;
