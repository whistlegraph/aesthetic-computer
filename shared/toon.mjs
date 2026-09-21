// toon.mjs — Token-Oriented Object Notation for tool replies.
//
// The house shape for any MCP tool that answers with a list. One header
// names the collection, its count and its columns; every row after it is
// bare comma-separated cells. Against the prose-and-bullet lists it replaces
// this reads at roughly two thirds the tokens, and the header makes an empty
// result definitive ("rocks[0]") instead of a sentence the model has to parse.
//
//   machines[4]{name,role,live}:
//     neo,control,up
//     blueberry,control,self
//
// Rules the rows follow (kept small enough to hold in the head):
//   • a cell is quoted only when it contains a comma, a quote, a newline, or
//     an edge space; an inner quote doubles ("")
//   • null and undefined print as nothing; numbers and booleans as themselves
//   • `[N of T]` marks a capped list so the caller knows there is more
//
// Node builtins only — this file is imported by the resident MCP daemons.

/** Escape one cell for a TOON row. */
export function cell(value) {
  if (value === null || value === undefined) return "";
  const s = typeof value === "string" ? value : String(value);
  return /[",\n\r]|^\s|\s$/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
}

/**
 * Render rows as a TOON block.
 *   name    collection name shown in the header ("papers")
 *   rows    array of plain objects
 *   fields  ordered column names; each row is read by these keys
 *   total   optional true size when `rows` is a capped slice
 *   note    optional trailing line (where to get more, what was hidden)
 */
export function toon(name, rows, fields, { total, note } = {}) {
  const n = rows.length;
  const count = total !== undefined && total !== n ? `${n} of ${total}` : String(n);
  const lines = [`${name}[${count}]{${fields.join(",")}}:`];
  for (const row of rows) lines.push("  " + fields.map((f) => cell(row[f])).join(","));
  if (note) lines.push(note);
  return lines.join("\n");
}

/** Clip a string with a size hint, so the reader knows how much was cut. */
export function clip(text, max = 80) {
  const s = String(text ?? "").replace(/\s+/g, " ").trim();
  return s.length <= max ? s : `${s.slice(0, max - 1)}…(+${s.length - max + 1}ch)`;
}

/**
 * Shorten an absolute path against named roots: { $AC: "/Users/x/ac" }.
 * The longest matching root wins, so a vault inside the repo prints as
 * $VAULT/… rather than $AC/aesthetic-computer-vault/….
 */
export function shortPath(p, roots = {}) {
  if (!p) return "";
  const s = String(p);
  const hit = Object.entries(roots)
    .filter(([, root]) => root && (s === root || s.startsWith(root.endsWith("/") ? root : root + "/")))
    .sort((a, b) => b[1].length - a[1].length)[0];
  if (!hit) return s;
  const [label, root] = hit;
  return label + s.slice(root.replace(/\/$/, "").length);
}

/** Compact age: 42s, 7m, 3h, 5d. */
export function ago(when, now = Date.now()) {
  const t = typeof when === "number" ? when : Date.parse(when);
  if (!Number.isFinite(t)) return "?";
  const s = Math.max(0, Math.round((now - t) / 1000));
  if (s < 90) return `${s}s`;
  const m = Math.round(s / 60);
  if (m < 90) return `${m}m`;
  const h = Math.round(m / 60);
  if (h < 48) return `${h}h`;
  return `${Math.round(h / 24)}d`;
}

/** ISO timestamp → "09-20 14:05" (drops the year and seconds), or "" */
export function shortWhen(iso) {
  if (!iso) return "";
  const s = String(iso);
  const m = s.match(/^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2}))?/);
  if (!m) return s;
  return m[4] ? `${m[2]}-${m[3]} ${m[4]}:${m[5]}` : `${m[2]}-${m[3]}`;
}
