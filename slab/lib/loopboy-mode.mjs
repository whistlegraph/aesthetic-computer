// Mutable session mode, separate from launch-time environment and live markers.
// An explicit empty contact persists an exit even if an old watcher rewrites
// its launch contact. No process, terminal, or provider history is touched.
import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";
import { randomUUID } from "node:crypto";

export function modePath(id, env = process.env) {
  if (!/^[A-Za-z0-9._-]{1,180}$/.test(id) || id === "." || id === "..") {
    throw new Error("Loopboy mode requires a valid session id");
  }
  return join(env.SLAB_HOME || join(homedir(), ".local/share/slab"), "state/loopboy-modes", `${id}.json`);
}

export async function readLoopboyMode(id, env) {
  let text;
  try { text = await readFile(modePath(id, env), "utf8"); }
  catch (error) { if (error.code === "ENOENT") return null; throw error; }
  try {
    const mode = JSON.parse(text);
    if (mode.sessionId === id && typeof mode.contact === "string") return mode;
  } catch {}
  // Fail closed without making one corrupt mode hide the entire fleet ledger.
  return { sessionId: id, contact: "", invalid: true };
}

export async function writeLoopboyMode(id, { contact = "", name = "" }, env) {
  if (contact && !/^[a-z0-9_-]{1,40}$/.test(contact)) throw new Error("Invalid Loopboy contact key");
  const file = modePath(id, env);
  await mkdir(join(file, ".."), { recursive: true, mode: 0o700 });
  const mode = { version: 1, sessionId: id, contact, name, changedAt: new Date().toISOString() };
  const temp = `${file}.${randomUUID()}.tmp`;
  await writeFile(temp, JSON.stringify(mode) + "\n", { mode: 0o600 });
  await rename(temp, file);
  return mode;
}
