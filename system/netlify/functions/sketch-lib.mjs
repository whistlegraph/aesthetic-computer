// Sketch lib, 2026.10.30
// The pure half of `/api/sketch`: what a generated drawing may contain, and
// how every drawing gets a tune. Kept apart from sketch.mjs so it can be
// tested without Mongo or a model.

// Generated code runs in visitors' browsers: drawing and a tune, no
// navigation, no mic, no links.
export const DENY = new Set(("hop jump embed paste stamp painting steal putback source cache mic amplitude speaker " +
  "overtone tap delay debug log label bake die write say sample stample").split(" "));

// A tune is a `melody` (or `clock`) line. Both play on the synced AC clock:
// one plain note is one second of UTC, and the loop position is the absolute
// time modulo the loop, so every visitor hears the same note at the same
// moment, and `1s`/`2s...` visual cycles land on the beat.
const TUNE = /\((melody|clock)\s+([^()]+)\)/g;

export function tunes(source) {
  return [...source.matchAll(TUNE)].map((m) => m[2].trim()).filter(Boolean);
}

export const musical = (source) => tunes(source).length > 0;

// Inside a melody string, braces pick a voice and a volume. Only oscillators
// and physical voices: `{say}`, `{sample}`, `{#code}` and `"speech"` reach
// out of the drawing.
const VOICES = "sine|triangle|sawtooth|saw|square|harp|pluck|guitar|string|whistle|flute|ocarina|noise|noise-white";
const BRACE = new RegExp(`^\\{(?:(?:${VOICES})(?::[0-9.]+)?|[0-9.]+|-?[0-9.]+hz&?)\\}$`);

function safeTune(tune) {
  const quotes = (tune.match(/"/g) || []).length;
  if (quotes && !(quotes === 2 && /^"[^"]*"$/.test(tune))) return false;
  return (tune.match(/\{[^}]*\}/g) || []).every((b) => BRACE.test(b));
}

// It must also parse: an operator against a paren (`h-(min w h)`, `)*(`) or a
// CSS colour function blanks the whole frame.
export function safe(source) {
  if (source.length > 4000 || /https?:|:\/\/|\$[a-z0-9]/i.test(source)) return false;
  if (/[\w.)][-+*/]\(|\b(rgba?|hsla?)\(/i.test(source)) return false;
  let depth = 0;
  for (const ch of source) {
    if (ch === "(") depth++;
    if (ch === ")" && --depth < 0) return false;
  }
  if (depth !== 0) return false;
  if (!tunes(source).every(safeTune)) return false;
  return !(source.match(/[a-z][a-z0-9_-]*/gi) || []).some((t) => DENY.has(t.toLowerCase()));
}

// The word as a tune: a..g are themselves, every other letter lands on the
// scale by its place in the alphabet, a space rests. `snake` → gbafe.
export function wordTune(word) {
  const notes = [...word.toLowerCase()].map((ch) => {
    if (ch === " ") return "_";
    if (ch >= "a" && ch <= "g") return ch;
    if (ch >= "h" && ch <= "z") return "cdefgab"[(ch.charCodeAt(0) - 97) % 7];
    return "";
  }).join("");
  const short = notes.replace(/_/g, "").length > 6 ? "." : "";
  return `{triangle:0.35} ${short}${notes}`;
}

// Every drawing sings: a model that forgot the tune gets the word's own, and
// loud voices come down to a volume that sits under a conversation.
const QUIET = 0.5;
export function tuned(word, program) {
  if (!program) return program;
  if (!musical(program)) return `${program.trim()}\n(melody "${wordTune(word)}")`;
  return program.replace(TUNE, (_, head, args) => {
    // Always quoted: a bare `{0.4}` is not a token KidLisp reads.
    let tune = args.trim().replace(/^"|"$/g, "").replace(/\{([a-z-]+:)?([0-9.]+)\}/g, (b, voice, vol) =>
      parseFloat(vol) > QUIET ? `{${voice || ""}${QUIET}}` : b);
    if (!/\{(?:[a-z-]+:)?[0-9.]+\}/.test(tune)) tune = `{0.4} ${tune}`;
    return `(${head} "${tune}")`;
  });
}

// One drawing per word at a time, owned by the server, not the request: the
// model keeps writing when the tab that asked closes, and a second visitor
// typing the same word mid-way joins the same drawing instead of starting
// another. `run(onText)` must settle to the result and never throw.
const inflight = new Map();
export function share(word, run) {
  if (inflight.has(word) || !run) return inflight.get(word) || null;
  const job = { text: "", listeners: new Set() };
  job.promise = run((t) => {
    job.text += t;
    for (const listen of job.listeners) try { listen(t); } catch {}
  }).finally(() => inflight.delete(word));
  inflight.set(word, job);
  return job;
}

// A job as a response body: what has been written so far, then every new
// piece, then `\n` + `end` + the JSON result. A reader that goes away is
// forgotten; the job is not.
export function streamFrom(job, end = "\u0000") {
  const encoder = new TextEncoder();
  let send = null;
  return new ReadableStream({
    start(controller) {
      send = (t) => { try { controller.enqueue(encoder.encode(t)); } catch {} };
      if (job.text) send(job.text);
      job.listeners.add(send);
      job.promise
        .then((result) => send(`\n${end}${JSON.stringify(result)}`), () => send(`\n${end}{"skip":true}`))
        .finally(() => { job.listeners.delete(send); try { controller.close(); } catch {} });
    },
    cancel() { if (send) job.listeners.delete(send); },
  });
}
