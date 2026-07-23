import { chmod, mkdir, readFile, readdir, rename, unlink, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";
import { randomUUID } from "node:crypto";

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

export const LOOPBOY_INBOX_ROOT = process.env.SLAB_LOOPBOY_INBOX || join(
  process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab"),
  "loopboy",
  "inbox",
);

function safeSessionId(value) {
  const sessionId = String(value || "").trim();
  if (!/^[A-Za-z0-9-]{8,128}$/.test(sessionId)) {
    throw new Error("Loopboy inbox requires a valid session id");
  }
  return sessionId;
}

function sessionDir(sessionId) {
  return join(LOOPBOY_INBOX_ROOT, safeSessionId(sessionId));
}

export async function enqueueLoopboyEvent(event) {
  const sessionId = safeSessionId(event?.sessionId);
  const dir = sessionDir(sessionId);
  await mkdir(dir, { recursive: true, mode: 0o700 });
  await chmod(dir, 0o700);
  const createdAt = event.createdAt || new Date().toISOString();
  const id = event.id || randomUUID();
  const payload = {
    version: 1,
    id,
    sessionId,
    contact: String(event.contact || ""),
    displayName: String(event.displayName || event.contact || ""),
    kind: event.kind === "heartbeat" ? "heartbeat" : "message",
    fromMe: event.fromMe === true,
    excerpt: String(event.excerpt || "").slice(0, 500),
    prompt: String(event.prompt || "").slice(0, 6000),
    createdAt,
  };
  const stamp = Date.now().toString().padStart(13, "0");
  const name = `${stamp}-${payload.kind}-${id}.json`;
  if (payload.kind === "heartbeat") {
    const oldHeartbeats = (await readdir(dir)).filter((entry) =>
      /^\d{13}-heartbeat-.*\.json$/.test(entry)
    );
    await Promise.all(oldHeartbeats.map((entry) => unlink(join(dir, entry)).catch(() => {})));
  }
  const temp = join(dir, `.${name}.${process.pid}.tmp`);
  const final = join(dir, name);
  await writeFile(temp, `${JSON.stringify(payload)}\n`, { mode: 0o600 });
  await rename(temp, final);
  return payload;
}

async function claimOldest(sessionId) {
  const dir = sessionDir(sessionId);
  let names = [];
  try {
    names = (await readdir(dir)).filter((name) => /^\d{13}-(?:heartbeat|message)-.*\.json$/.test(name)).sort();
  } catch (error) {
    if (error?.code === "ENOENT") return null;
    throw error;
  }
  for (const name of names) {
    const source = join(dir, name);
    const claimed = join(dir, `.claimed-${process.pid}-${randomUUID()}.json`);
    try {
      await rename(source, claimed);
    } catch (error) {
      if (error?.code === "ENOENT") continue;
      throw error;
    }
    try {
      const event = JSON.parse(await readFile(claimed, "utf8"));
      if (event.sessionId !== safeSessionId(sessionId)) {
        throw new Error("Loopboy inbox event/session mismatch");
      }
      return event;
    } finally {
      await unlink(claimed).catch(() => {});
    }
  }
  return null;
}

export async function waitLoopboyEvent(sessionId, { timeoutMs = 50_000, pollMs = 200 } = {}) {
  const sid = safeSessionId(sessionId);
  const deadline = Date.now() + Math.max(0, Math.min(55_000, Number(timeoutMs) || 0));
  do {
    const event = await claimOldest(sid);
    if (event) return event;
    if (Date.now() >= deadline) return null;
    await sleep(Math.max(50, Math.min(1000, Number(pollMs) || 200)));
  } while (true);
}
