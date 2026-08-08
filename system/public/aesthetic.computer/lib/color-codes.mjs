// 🎨 Inline color codes — the `\color\text\reset\` markup that `write` reads.

// One scanner, shared by the renderer (`disk.mjs`) and by every producer that
// splices colors into text it did not author (chat, hotlink, moods, prompt).
// The producers own the markup; the text they wrap around is often somebody
// else's typing, so it gets escaped on the way in: `\\` is a literal
// backslash. That is the whole trick — a user who types `\red\hi\reset\` sees
// those characters instead of red text, and nothing is silently eaten.

// Walk a string once, splitting it into rendered text and color codes.
// Returns the alternating array [text, code, text, code, …, text] — always
// odd length, always text first, the same shape `String.split` gave us before.
// A `\` with no closing `\` after it is just a backslash, not a broken code.
export function splitColorCodes(str) {
  if (!str) return [str || ""];
  const parts = [""];
  let i = 0;
  while (i < str.length) {
    if (str[i] !== "\\") {
      parts[parts.length - 1] += str[i];
      i += 1;
    } else if (str[i + 1] === "\\") {
      parts[parts.length - 1] += "\\"; // an escaped backslash renders as one
      i += 2;
    } else {
      const close = str.indexOf("\\", i + 1);
      if (close === -1) {
        parts[parts.length - 1] += "\\"; // unterminated — just a character
        i += 1;
      } else {
        parts.push(str.slice(i + 1, close), "");
        i = close + 1;
      }
    }
  }
  return parts;
}

// What the text looks like once the codes are gone — the string to measure so
// a measured width matches a drawn width.
export function stripColorCodes(str) {
  if (!str) return str;
  const parts = splitColorCodes(str);
  let out = "";
  for (let i = 0; i < parts.length; i += 2) out += parts[i];
  return out;
}

// Is there a real `\code\` pair in here? (Used to sniff kidlisp-ish text.)
export function hasColorCodes(str) {
  if (!str) return false;
  return splitColorCodes(str).length > 1;
}

// Make text safe to splice next to color codes.
export function escapeColorCodes(str) {
  if (!str) return str || "";
  return str.replace(/\\/g, "\\\\");
}

// Escaping shifts every character right of a backslash, so a producer that
// holds offsets into the raw string needs to translate them. `map[i]` is where
// raw character `i` landed in `escapeColorCodes(str)`; `map[str.length]` is the
// end, so a raw [start, end) slice maps straight across.
export function escapedIndexMap(str) {
  const map = new Array((str?.length || 0) + 1);
  let shift = 0;
  for (let i = 0; i < (str?.length || 0); i++) {
    map[i] = i + shift;
    if (str[i] === "\\") shift += 1;
  }
  map[str?.length || 0] = (str?.length || 0) + shift;
  return map;
}

// Rewrite every code in place (shadow passes do this), leaving the text alone.
// Return `null` from `fn` to drop a code entirely.
export function mapColorCodes(str, fn) {
  if (!str) return str;
  const parts = splitColorCodes(str);
  let out = escapeColorCodes(parts[0]);
  for (let i = 1; i < parts.length; i += 2) {
    const replacement = fn(parts[i]);
    if (replacement !== null && replacement !== undefined) out += `\\${replacement}\\`;
    out += escapeColorCodes(parts[i + 1]);
  }
  return out;
}
