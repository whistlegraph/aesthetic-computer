// A prox signature: the sending rock's `host:name`, carried invisibly at the
// end of an outgoing message in Unicode tag characters (U+E0020–E007E mirror
// printable ASCII; U+E0001 opens, U+E007F closes). Nothing renders, but the
// characters ride along through copy and paste — so a pasted agent reply can
// be traced back to the session that wrote it.

const OPEN = "\u{E0001}";
const CLOSE = "\u{E007F}";
const PREFIX = "prox:";
const TAGGED = /\u{E0001}([\u{E0020}-\u{E007E}]+)\u{E007F}/gu;

const hide = (ascii) => Array.from(ascii, (c) => String.fromCodePoint(0xe0000 + c.charCodeAt(0))).join("");
const show = (tags) => Array.from(tags, (c) => String.fromCharCode(c.codePointAt(0) - 0xe0000)).join("");

// `handle` is a rock's `host:name`; only printable ASCII can be carried.
export function sign(text, handle) {
  const body = String(text || "");
  if (!body || !handle || !/^[\x20-\x7e]+$/.test(handle)) return body;
  return `${unsign(body)}${OPEN}${hide(PREFIX + handle)}${CLOSE}`;
}

// Every signature in `text`, as `host:name` handles (newest-pasted last).
export function signatures(text) {
  return [...String(text || "").matchAll(TAGGED)]
    .map((m) => show(m[1]))
    .filter((s) => s.startsWith(PREFIX))
    .map((s) => s.slice(PREFIX.length));
}

export function unsign(text) {
  return String(text || "").replace(TAGGED, "");
}

// Make signatures visible for a reader: each becomes ` ⟨prox host:name⟩`.
export function revealSignatures(text) {
  return String(text || "").replace(TAGGED, (whole, tags) => {
    const s = show(tags);
    return s.startsWith(PREFIX) ? ` ⟨prox ${s.slice(PREFIX.length)}⟩` : whole;
  });
}
