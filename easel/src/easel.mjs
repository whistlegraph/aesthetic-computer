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
// Painted, because the thing being stood up is a painting surface and a grey
// one would be a lie about what the session is for. The name arrives in the
// palette's colours, one hue per character, so writing it on reads as laying
// down paint rather than typing. The frame stays quiet underneath it — a loud
// frame competes with the canvas, which is the one thing here worth looking at.
//
// Pure, like the mascot. A caller asks what the easel looks like at a given
// number of milliseconds and gets lines back. Nothing schedules, nothing draws,
// nothing reads a clock. Colour arrives the same way: the caller passes the
// escapes it wants used and gets them back inside the lines, so this module
// still knows nothing about terminals, and a caller that passes none — a test,
// a pipe — gets the same plain frame it always did.

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

// The roles a caller may colour. Anything it leaves out is simply not painted,
// so a partial ink map is as valid as a full one and an absent one is plain
// text — which is what keeps the frame measurable (see `easelWidth`).
const ROLES = ["frame", "name", "address", "cursor", "legs", "reset"];

function inks(ink) {
  const out = {};
  // `name` keeps its shape — it is the one role that may be a list of hues, and
  // flattening it to a string would print the separators.
  for (const role of ROLES) {
    const tone = ink?.[role];
    out[role] = Array.isArray(tone) ? tone : String(tone ?? "");
  }
  return out;
}

// Paint one span and hand the colour back, so the next span starts from a
// known state rather than inheriting whatever the last one left running.
function paint(text, tone, reset) {
  return tone ? `${tone}${text}${reset}` : text;
}

// The name is laid down one character per hue, cycling. `ink.name` may be a
// single escape (the whole name in one colour) or an array of them, which is
// where the painted look comes from.
function paintName(text, name, reset) {
  if (!name || name.length === 0) return text;
  const hues = Array.isArray(name) ? name : [name];
  return Array.from(text)
    .map((character, index) => paint(character, hues[index % hues.length], reset))
    .join("");
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

// How wide the easel stands, in columns. A painted frame carries escapes that
// occupy no columns, so a caller that needs to centre it has to ask rather than
// measure `lines[0].length`.
export function easelWidth(piece = "", address = "") {
  return innerWidth(String(piece || ""), String(address || "")) + 2;
}

// One frame. `piece` is the slug as it appears on disk; `address` is the bare
// host+path the rock is showing. Either may be empty — a session that is not
// signed in has no address yet, and the canvas simply stays blank there rather
// than apologising for it.
//
// `ink` is optional and supplies the escapes for each role — `name` may be an
// array, which paints the name a hue per character. Without it the frame comes
// back as plain text.
export function easelFrame(elapsed = 0, { piece = "", address = "" } = {}, ink) {
  const name = String(piece || "");
  const where = String(address || "");
  const inner = innerWidth(name, where);
  const shown = revealed(elapsed, name, where);
  const tone = inks(ink);

  // A cursor while the name is still being written, so the pause between the
  // name landing and the address starting does not read as a hang. It is its
  // own colour because it is the interface talking, not the piece.
  const writing = shown.piece < name.length;
  const written = name.slice(0, shown.piece);
  const nameText = written + (writing ? "▌" : "");
  const nameInk =
    paintName(written, tone.name, tone.reset) +
    (writing ? paint("▌", tone.cursor, tone.reset) : "");

  // Every row is padded before it is painted, so the escapes land inside a
  // frame whose columns were already counted.
  const bar = (edge, fill, middle) =>
    paint(edge[0] + fill + middle + edge[1], tone.frame, tone.reset);
  const edge = paint("│", tone.frame, tone.reset);
  // The same arithmetic `centre` uses, kept here rather than searching the
  // padded string for the text — a name that starts with a space would find
  // itself in the padding.
  const row = (text, painted) => {
    const width = Math.min(text.length, inner);
    const left = Math.floor((inner - width) / 2);
    return (
      edge +
      " ".repeat(left) +
      painted +
      " ".repeat(inner - width - left) +
      edge
    );
  };

  const top = bar("┌┐", "─".repeat(inner), "");
  const blank = row("", "");

  // The bottom edge carries the mast the legs hang from, so the easel reads as
  // one object rather than a box resting on two sticks.
  const mastAt = Math.floor(inner / 2);
  const bottom = bar(
    "└┘",
    "─".repeat(mastAt) + "┬" + "─".repeat(inner - mastAt - 1),
    "",
  );

  const pad = " ".repeat(mastAt);
  return [
    top,
    blank,
    row(nameText, nameInk),
    blank,
    row(where.slice(0, shown.address), paint(where.slice(0, shown.address), tone.address, tone.reset)),
    bottom,
    pad + paint("╱ ╲", tone.legs, tone.reset),
    pad.slice(0, Math.max(0, mastAt - 1)) + paint("╱   ╲", tone.legs, tone.reset),
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
