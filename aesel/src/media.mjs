// Mime awareness: which media file a session has its hands on.
//
// A pro session has no piece, so nothing tells the Slab card what to show.
// What it has instead is a stream of tool calls, and the paths in them say
// what is being made: the png a brush just wrote, the mp4 ffmpeg just
// finished. This reads those paths out of a tool's input and output, keeps
// the ones that are real files of a kind a card can show, and puts the
// freshest first.
import { statSync } from "node:fs";
import { homedir } from "node:os";
import path from "node:path";

export const MEDIA_TYPES = new Map([
  ["png", { kind: "picture", mime: "image/png", glyph: "🖼" }],
  ["jpg", { kind: "picture", mime: "image/jpeg", glyph: "🖼" }],
  ["jpeg", { kind: "picture", mime: "image/jpeg", glyph: "🖼" }],
  ["webp", { kind: "picture", mime: "image/webp", glyph: "🖼" }],
  ["wav", { kind: "sound", mime: "audio/wav", glyph: "🔊" }],
  ["mp3", { kind: "sound", mime: "audio/mpeg", glyph: "🔊" }],
  ["pdf", { kind: "paper", mime: "application/pdf", glyph: "📄" }],
  ["mp4", { kind: "video", mime: "video/mp4", glyph: "🎬" }],
  ["m4v", { kind: "video", mime: "video/mp4", glyph: "🎬" }],
  ["mov", { kind: "video", mime: "video/quicktime", glyph: "🎬" }],
  ["webm", { kind: "video", mime: "video/webm", glyph: "🎬" }],
]);

// Paths end where shell syntax, quotes, JSON punctuation or whitespace begin.
// A colon is a separator too: `out.png:12` is a location, not a file.
const BREAKS = /[\s"'`<>|;&(),=:\[\]{}]+/;

// Every media file named in the text that exists, freshest first.
export function mediaPaths(text, cwd, { home = homedir() } = {}) {
  const found = [];
  const seen = new Set();
  for (const raw of String(text || "").split(BREAKS)) {
    const token = raw.replace(/[.!?]+$/, "");
    const type = MEDIA_TYPES.get(path.extname(token).slice(1).toLowerCase());
    if (!type) continue;
    const expanded = token.startsWith("~/") ? path.join(home, token.slice(2)) : token;
    const absolute = path.resolve(cwd, expanded);
    if (seen.has(absolute)) continue;
    seen.add(absolute);
    let info;
    try { info = statSync(absolute); } catch { continue; }
    if (!info.isFile() || info.size === 0) continue;
    found.push({ path: absolute, name: path.basename(absolute), ...type, mtimeMs: info.mtimeMs, size: info.size });
  }
  return found.sort((a, b) => b.mtimeMs - a.mtimeMs);
}

// The text of a tool item worth reading for paths: what it was told, what it
// touched, and the tail of what it said back.
export function itemText(item) {
  if (!item) return "";
  return [
    item.command,
    item.tool,
    item.path,
    ...(item.changes || []).map((change) => change?.path),
    item.input ? JSON.stringify(item.input) : "",
    typeof item.aggregatedOutput === "string" ? item.aggregatedOutput.slice(-4000) : "",
  ].filter(Boolean).join("\n");
}

// Whether the next sighting replaces the current one: a different file, or
// the same file written again since.
export function mediaChanged(current, next) {
  if (!next) return false;
  if (!current) return true;
  return current.path !== next.path || current.mtimeMs !== next.mtimeMs;
}
