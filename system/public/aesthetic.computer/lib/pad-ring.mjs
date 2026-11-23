// 🎮 Pad Ring
// Typing at the prompt with only a pad, as rax's build ring composes a build
// (rax/radial.mjs): the left stick points into a ring of words, and only the
// words that make a valid command at this point are on it. First the
// commands and pieces (/docs.json, the most used first), then, once one is
// written, the values its next parameter takes (a `values` list, or the
// pieces for a `<piece>`). A parameter with no list of values is free text:
// the ring is empty and the letter wheel spells it.
//
// The letter wheel is the right stick: it points at one of eight petals and
// the face buttons type the petal's four signs, each in the place of its
// button (Y top, B right, A bottom, X left). LT held turns the wheel to the
// digits and signs.
//
// This file only reckons: lib/pad-cursor.mjs reads the pad and types, and
// lib/pad-deck.mjs draws.

const { atan2, hypot, PI, round } = Math;

export const SLICES = 12; // Words on the ring at once; LB turns to the next twelve.
export const AIM = 0.5; // How far a stick is pushed before it points at a slice.

// Petal signs in button order: Y (top), B (right), A (bottom), X (left).
export const PETALS = ["abcd", "efgh", "ijkl", "mnop", "qrst", "uvwx", "yz.'", ":~-/"];
export const SIGNS = ["1234", "5678", "90,!", "?@$#", "+=*_", "()[]", "<>&%", '"|\\;'];
export const FACE = { 3: 0, 1: 1, 0: 2, 2: 3 }; // Button index → place in a petal.

// The slice a stick points at, first at the top and clockwise, or -1.
// Pad axes have y down.
export function aim(x, y, count, threshold = AIM) {
  if (!count || hypot(x, y) < threshold) return -1;
  const turn = (atan2(x, -y) / (2 * PI) + 1) % 1;
  return round(turn * count) % count;
}

// What can be written, from /docs.json: name → [{ values, piece, repeats }].
export function grammar(docs, hits = []) {
  const words = new Map();
  const rank = new Map(hits.map((h, i) => [h.piece || h.name || h, i]));
  for (const group of ["prompts", "pieces"]) {
    for (const [name, doc] of Object.entries(docs?.[group] || {})) {
      if (doc?.hidden || words.has(name) || /\s/.test(name)) continue;
      words.set(name, params(doc));
    }
  }
  const names = [...words.keys()].sort((a, b) =>
    (rank.get(a) ?? 1e9) - (rank.get(b) ?? 1e9) || a.length - b.length || a.localeCompare(b));
  const pieces = names.filter((n) => docs?.pieces?.[n]);
  return { words, names, pieces };
}

// A doc's parameters: its `params` where they are given, else read from its
// `sig` ("edit <piece>", "channel [name]"). Only values that can be listed
// are kept; anything else is free text (null values).
function params(doc) {
  if (Array.isArray(doc.params) && doc.params.length) {
    return doc.params.map((p) => ({
      values: Array.isArray(p.values) ? p.values.map(String) : null,
      piece: !p.values && /piece/i.test(`${p.name} ${p.type}`),
      repeats: p.type === "flags",
      optional: p.required === false,
    }));
  }
  const slots = String(doc.sig || "").match(/[<[][^>\]]+[>\]]/g) || [];
  return slots.map((s) => ({ values: null, piece: /piece/i.test(s), repeats: false, optional: s[0] === "[" }));
}

// The words that may follow `text` (the prompt line up to the caret), the
// part of the last word already written, and what picking each one writes.
export function offer(g, text = "") {
  if (!g) return { partial: "", words: [] };
  if (/^\s*[(;]/.test(text)) return { partial: "", words: [] }; // KidLisp: free text.
  const tokens = text.replace(/^\s+/, "").split(/\s+/);
  const partial = tokens.pop() || "";
  const starts = (w) => w.startsWith(partial) && w !== partial;
  if (!tokens.length) return { partial, words: g.names.filter(starts) };
  const [head, ...done] = tokens;
  const ps = g.words.get(head) || g.words.get(head.split(":")[0]);
  if (!ps?.length) return { partial, words: [] };
  // The next parameter's words; an optional one with nothing to list (a
  // number, a name) may be skipped, so the one after it is offered as well.
  const words = [];
  for (let i = done.length; ; i++) {
    const p = ps[i] || (ps.at(-1).repeats ? ps.at(-1) : null);
    if (!p) break;
    const pool = p.values || (p.piece ? g.pieces : []);
    for (const w of pool) if (starts(w) && !(p.repeats && done.includes(w)) && !words.includes(w)) words.push(w);
    if (pool.length || !p.optional || !ps[i + 1]) break;
  }
  return { partial, words };
}

// Whether a word takes more after it (a space is written after it then).
export function more(g, text, word) {
  const tokens = text.replace(/^\s+/, "").split(/\s+/);
  if (tokens.length <= 1) return !!g?.words.get(word)?.length;
  const ps = g.words.get(tokens[0]) || g.words.get(tokens[0].split(":")[0]) || [];
  return tokens.length - 1 < ps.length || !!ps.at(-1)?.repeats;
}
