// mime.mjs — post a file you made to mime.ac, as your @handle.
//
// mime.ac is AC's discussion layer over media (system/backend/MIME.md). Its
// API already takes a native upload as an opening post: `POST /api/mime` with
// `{ parent: null, text, file: { name, type, data } }`, base64 bytes, 8 MiB at
// most. A bearer token makes the post the signed-in account's; the server
// resolves the handle from it, never from anything sent here. The board is
// the file's MIME type. This is the client half, shared by `ac mime` and
// Aesel's media list.
import { readFileSync, statSync } from "node:fs";
import path from "node:path";
import { SITE } from "./ac-session.mjs";

export const MIME_HOME = "https://mime.ac";
export const MAX_BYTES = 8 * 1024 * 1024;
const MAX_CAPTION = 4000;

// The type to declare; the server re-checks it and falls back on the extension.
const TYPES = {
  png: "image/png", jpg: "image/jpeg", jpeg: "image/jpeg", gif: "image/gif", webp: "image/webp", svg: "image/svg+xml",
  mp3: "audio/mpeg", wav: "audio/wav", ogg: "audio/ogg", flac: "audio/flac", m4a: "audio/mp4", mid: "audio/midi", midi: "audio/midi",
  mp4: "video/mp4", m4v: "video/mp4", mov: "video/quicktime", webm: "video/webm",
  pdf: "application/pdf", zip: "application/zip", json: "application/json", wasm: "application/wasm",
  txt: "text/plain", md: "text/markdown", html: "text/html", css: "text/css",
  js: "text/javascript", mjs: "text/javascript", lisp: "text/x-lisp", lua: "text/x-lua",
  ttf: "font/ttf", otf: "font/otf", woff: "font/woff", woff2: "font/woff2",
};
export const mimeType = (file) => TYPES[path.extname(file).slice(1).toLowerCase()] || "application/octet-stream";

// What would be posted, checked before any bytes leave: a real file, not
// empty, under the limit. Throws with a sentence a person can act on.
export function planMime(file, { cwd = process.cwd(), caption = "" } = {}) {
  const absolute = path.resolve(cwd, String(file || ""));
  let info;
  try { info = statSync(absolute); } catch { throw new Error(`no such file: ${file}`); }
  if (!info.isFile()) throw new Error(`not a file: ${file}`);
  if (info.size === 0) throw new Error(`${path.basename(absolute)} is empty`);
  if (info.size > MAX_BYTES) {
    throw new Error(`${path.basename(absolute)} is ${(info.size / 1048576).toFixed(1)} MB; mime.ac takes up to 8 MB (try an mp3 or a smaller export)`);
  }
  return { path: absolute, name: path.basename(absolute), type: mimeType(absolute), size: info.size, caption: String(caption || "").trim().slice(0, MAX_CAPTION) };
}

export const threadUrl = (code) => `${MIME_HOME}/#/t/${code}`;

// Post it. `session` is an ACSession: its token signs the post, so the
// server credits the account behind it. Returns the thread's code, board
// and address.
export async function postMime(plan, { session, fetchImpl = fetch, site = SITE } = {}) {
  if (!session?.signedIn) throw new Error("sign in first: ac login");
  const token = await session.token();
  const data = readFileSync(plan.path).toString("base64");
  const response = await fetchImpl(`${site}/api/mime`, {
    method: "POST",
    headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}` },
    body: JSON.stringify({ parent: null, text: plan.caption, file: { name: plan.name, type: plan.type, data } }),
  });
  const body = await response.json().catch(() => ({}));
  if (!response.ok || !body.code) throw new Error(body.error || body.message || `mime.ac answered ${response.status}`);
  return { code: body.code, board: body.board, url: threadUrl(body.code) };
}
