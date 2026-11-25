// utilitext — text that is only as long as it is useful.
//
// An agent reply opens with its signal: one line saying what happened or
// what is needed. A human decision goes on an `ask:` line. A few short points
// may follow. Everything else (reasoning, evidence, logs) goes inside
// <explain>…</explain>, which a UI folds until someone asks for it.
//
// The prompt half asks a model to write that shape; the filter half reads any
// reply (shaped or not) and shows it at a level. `step` moves the level as a
// person expands or asks for less, so verbosity is tuned while you work.

export const LEVELS = ["signal", "brief", "full"];

const SIGNAL_WORDS = 16;
const POINTS = 3;

/** The prompt clause that asks a model for utilitext at a level. */
export function utilitextPrompt(level = "brief") {
  const lines = [
    `Open with one line of at most ${SIGNAL_WORDS} words: what happened, or what you need. No preamble.`,
    "If a person must decide something, add one line starting `ask:` with the choice.",
  ];
  if (level !== "signal") lines.push(`Then at most ${POINTS} short points, one fact each. Do not restate the first line.`);
  lines.push(
    level === "full"
      ? "Put reasoning, evidence and logs inside <explain>…</explain> after the points."
      : "Put any reasoning, evidence or logs inside <explain>…</explain>; they stay folded unless asked for.",
    "No headings, no summaries of what you just said, no sign-off.",
  );
  return lines.join("\n");
}

const EXPLAIN = /<explain>([\s\S]*?)(?:<\/explain>|$)/gi;
const POINT = /^\s*(?:[-*•]|\d+[.)])\s+/;

/** Split a reply into signal, ask, points and explain. Unshaped replies still read. */
export function read(reply = "") {
  const explain = [...reply.matchAll(EXPLAIN)].map((m) => m[1].trim()).filter(Boolean).join("\n\n");
  const lines = reply.replace(EXPLAIN, "").split("\n").map((l) => l.trimEnd()).filter((l) => l.trim());
  let ask = "";
  const kept = [];
  for (const line of lines) {
    const m = line.match(/^\s*ask:\s*(.+)$/i);
    if (m && !ask) ask = m[1].trim();
    else kept.push(line);
  }
  const first = kept.findIndex((l) => !/^\s*#/.test(l));
  const signal = first < 0 ? "" : clip(kept[first].replace(POINT, "").replace(/\*\*/g, "").trim());
  const after = kept.slice(first + 1);
  const points = after.filter((l) => POINT.test(l)).map((l) => l.replace(POINT, "").trim());
  const prose = after.filter((l) => !POINT.test(l) && !/^\s*#/.test(l)).join("\n").trim();
  return { signal, ask, points, explain: [prose, explain].filter(Boolean).join("\n\n") };
}

// A signal that ran long is cut at a word, so a summary row stays one line.
function clip(line, words = SIGNAL_WORDS * 2) {
  const w = line.split(/\s+/);
  return w.length <= words ? line : w.slice(0, words).join(" ") + " …";
}

/** A reply as shown at a level. */
export function filter(reply, level = "brief") {
  const r = typeof reply === "string" ? read(reply) : reply;
  const out = [r.signal];
  if (r.ask) out.push(`ask: ${r.ask}`);
  if (level !== "signal") out.push(...r.points.slice(0, level === "full" ? undefined : POINTS).map((p) => `- ${p}`));
  if (level === "full" && r.explain) out.push("", r.explain);
  else if (r.explain || (level === "signal" && r.points.length)) out.push("▸ explain");
  return out.filter((l, i) => l || i > 0).join("\n");
}

/** One line per agent; agents waiting on a person come first. */
export function summary(agents = []) {
  const rows = agents.map(({ name, reply = "", status = "" }) => ({ name, status, ...read(reply) }));
  rows.sort((a, b) => Number(!!b.ask) - Number(!!a.ask));
  return rows
    .map((r) => [r.name, r.status && `(${r.status})`, "·", r.ask ? `ask: ${r.ask}` : r.signal].filter(Boolean).join(" "))
    .join("\n");
}

/** Move a level by what the person did: "explain"/"more" steps up, "shorter"/"less" steps down. */
export function step(level, feedback) {
  const at = Math.max(0, LEVELS.indexOf(level));
  if (/^(explain|more|why|expand)/i.test(feedback)) return LEVELS[Math.min(at + 1, LEVELS.length - 1)];
  if (/^(shorter|less|tldr|brief)/i.test(feedback)) return LEVELS[Math.max(at - 1, 0)];
  return LEVELS[at];
}
