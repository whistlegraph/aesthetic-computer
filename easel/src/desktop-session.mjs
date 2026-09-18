// Private local desktop continuity. Explicit fields only: never serialize an
// engine, account session, environment, token resolver, or provider credential.
import { randomUUID } from "node:crypto";
import { mkdir, readFile, rename, rm, writeFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";
const clone = (value) => JSON.parse(JSON.stringify(value));
const LIMIT = 32 * 1024 * 1024;
export function desktopSnapshot({ cwd, backend, model, effort = "", live, state, options, engine, handoff = "", archivedConversation = [] }) {
  return clone({ schema: 1, cwd: resolve(cwd), savedAt: new Date().toISOString(), backend, model, effort,
    live: { file: live.file, runtime: live.runtime?.id || live.runtime, channel: live.fallbackChannel || live.channel },
    ui: Object.fromEntries(["entries", "input", "cursor", "history", "historyIndex", "queued", "medium", "livePaused", "showQr", "autoAllow", "scrollOffset"].map((key) => [key, state[key]]).filter(([, value]) => value !== undefined)),
    options: { autopublish: options.autopublish, mouseEnabled: options.mouseEnabled },
    engine: { threadId: engine.threadId || "", ...(backend === "ac" ? { messages: engine.messages || [], turns: engine.turns || 0 } : {}) }, handoff, archivedConversation });
}
export function validateDesktopSession(snapshot, cwd) {
  if (snapshot?.schema !== 1 || snapshot.cwd !== resolve(cwd) || !["ac", "claude", "codex"].includes(snapshot.backend)) throw new Error("Invalid desktop session or workspace mismatch.");
  if (typeof snapshot.model !== "string" || typeof snapshot.live?.file !== "string" || !["mjs", "lisp", "lua"].includes(snapshot.live.runtime)) throw new Error("Invalid desktop piece or model.");
  if (snapshot.artifactId !== undefined && (!/^[a-f0-9-]{36}$/.test(snapshot.artifactId) || !Number.isSafeInteger(snapshot.artifactVersion) || snapshot.artifactVersion < 1)) throw new Error("Invalid saved artifact reference.");
  if (snapshot.effort !== undefined && (typeof snapshot.effort !== "string" || !/^[a-z0-9-]{0,40}$/.test(snapshot.effort))) throw new Error("Invalid saved effort.");
  const ui = snapshot.ui;
  if (!ui || !Array.isArray(ui.entries) || !ui.entries.every((e) => e && typeof e.id === "string" && typeof e.kind === "string" && typeof e.text === "string") || typeof ui.input !== "string" || !Array.isArray(ui.history) || !Array.isArray(ui.queued) || ![...ui.history, ...ui.queued].every((v) => typeof v === "string") || !Number.isInteger(ui.cursor) || ui.cursor < 0 || ui.cursor > Array.from(ui.input).length) throw new Error("Invalid desktop transcript or draft.");
  if (typeof snapshot.engine?.threadId !== "string" || (snapshot.backend === "ac" && (!Array.isArray(snapshot.engine.messages) || !Number.isSafeInteger(snapshot.engine.turns) || snapshot.engine.turns < 0))) throw new Error("Invalid desktop engine history.");
  if (typeof snapshot.handoff !== "string" || !Array.isArray(snapshot.archivedConversation)) throw new Error("Invalid desktop handoff context.");
  return snapshot;
}
export async function readDesktopSession(file, cwd) {
  if (!file) return null;
  let bytes;
  try { bytes = await readFile(file); } catch (error) { if (error.code === "ENOENT") return null; throw error; }
  if (bytes.length > LIMIT) throw new Error("Desktop session exceeds its size limit.");
  return validateDesktopSession(JSON.parse(bytes.toString("utf8")), cwd);
}
async function atomicJson(file, value) {
  const bytes = JSON.stringify(value);
  if (Buffer.byteLength(bytes) > LIMIT) throw new Error("Desktop session exceeds its size limit.");
  await mkdir(dirname(file), { recursive: true, mode: 0o700 });
  const temporary = `${file}.${randomUUID()}.tmp`;
  try { await writeFile(temporary, bytes + "\n", { mode: 0o600, flag: "wx" }); await rename(temporary, file); }
  finally { await rm(temporary, { force: true }); }
}
export async function writeDesktopSession(file, snapshot) {
  if (!file) return;
  validateDesktopSession(snapshot, snapshot.cwd);
  await atomicJson(file, snapshot);
}
export function restoreDesktopEngine(engine, snapshot) {
  if (!snapshot) return;
  engine.threadId = snapshot.engine.threadId;
  if (snapshot.backend === "ac") { engine.messages = clone(snapshot.engine.messages); engine.turns = snapshot.engine.turns; }
}
export async function writeDesktopControl({ sessionPath, controlPath, snapshot, action }) {
  if (!sessionPath || !controlPath || !["restart", "update", "home"].includes(action)) throw new Error("Desktop restart/update is not configured.");
  // A request must never exist unless the exact resumable state was saved.
  await writeDesktopSession(sessionPath, snapshot);
  await atomicJson(controlPath, { action });
}
export async function readDesktopIntent(file) {
  if (!file) return "restart";
  let input;
  try { input = JSON.parse(await readFile(file, "utf8")); }
  catch (error) { if (error.code === "ENOENT") return "restart"; throw error; }
  await rm(file, { force: true });
  if (!["restart", "update", "home"].includes(input?.action)) throw new Error("Invalid desktop action.");
  return input.action;
}
