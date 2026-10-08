// Mime awareness: which media file a session has its hands on.
//
// A pro session has no piece, so nothing tells the Slab card what to show.
// What it has instead is a stream of tool calls, and the paths in them say
// what is being made: the png a brush just wrote, the mp4 ffmpeg just
// finished. This reads paths explicitly named in a tool's input, keeps
// the ones that are real files of a kind a card can show, and puts the
// freshest first.
import { statSync, watch } from "node:fs";
import { homedir } from "node:os";
import path from "node:path";

export const MEDIA_TYPES = new Map([
  ["png", { kind: "picture", mime: "image/png", glyph: "🖼" }],
  ["jpg", { kind: "picture", mime: "image/jpeg", glyph: "🖼" }],
  ["jpeg", { kind: "picture", mime: "image/jpeg", glyph: "🖼" }],
  ["webp", { kind: "picture", mime: "image/webp", glyph: "🖼" }],
  ["svg", { kind: "picture", mime: "image/svg+xml", glyph: "🖼" }],
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

// One path, taken whole (spaces and all): the media file it names, or null
// when it is missing, empty, or not a kind a card can show.
export function mediaFile(file, cwd, { home = homedir() } = {}) {
  const token = String(file || "").trim();
  const type = MEDIA_TYPES.get(path.extname(token).slice(1).toLowerCase());
  if (!type) return null;
  const expanded = token.startsWith("~/") ? path.join(home, token.slice(2)) : token;
  const absolute = path.resolve(cwd, expanded);
  let info;
  try { info = statSync(absolute); } catch { return null; }
  if (!info.isFile() || info.size === 0) return null;
  return { path: absolute, name: path.basename(absolute), ...type, mtimeMs: info.mtimeMs, size: info.size };
}

// Every media file named in the text that exists, freshest first.
export function mediaPaths(text, cwd, { home = homedir() } = {}) {
  const found = [];
  const seen = new Set();
  for (const raw of String(text || "").split(BREAKS)) {
    const media = mediaFile(raw.replace(/[.!?]+$/, ""), cwd, { home });
    if (!media || seen.has(media.path)) continue;
    seen.add(media.path);
    found.push(media);
  }
  return found.sort((a, b) => b.mtimeMs - a.mtimeMs);
}

// Only explicit tool targets can nominate a preview. Output is evidence,
// not a selection: git status, searches and source reads can list unrelated
// media already on disk.
export function itemText(item) {
  if (!item) return "";
  return [
    item.command,
    item.tool,
    item.path,
    ...(item.changes || []).map((change) => change?.path),
    item.input ? JSON.stringify(item.input) : "",
  ].filter(Boolean).join("\n");
}

// Follow one media file as it is written again: a render finishing, a master
// replaced. Watches the directory rather than the file, because a renderer
// that writes beside and renames (the safe way) swaps the file's inode out
// from under a file watch. Calls onChange with the fresh sighting once the
// writes settle. Returns a function that stops watching.
export function watchMedia(media, onChange, { settleMs = 200 } = {}) {
  let last = media.mtimeMs, timer = null, watcher;
  const check = () => {
    timer = null;
    const next = mediaFile(media.path, "/");
    if (!next || next.mtimeMs === last) return;
    last = next.mtimeMs;
    onChange(next);
  };
  try {
    watcher = watch(path.dirname(media.path), (event, name) => {
      if (name && name !== media.name) return;
      clearTimeout(timer);
      timer = setTimeout(check, settleMs);
    });
  } catch { return () => {}; }
  watcher.on("error", () => {});
  return () => { clearTimeout(timer); watcher.close(); };
}

// Whether the next sighting replaces the current one: a different file, or
// the same file written again since.
export function mediaChanged(current, next) {
  if (!next) return false;
  if (!current) return true;
  return current.path !== next.path || current.mtimeMs !== next.mtimeMs;
}
