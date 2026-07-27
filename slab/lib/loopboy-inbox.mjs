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
export const LOOPBOY_BUS_ROOT = process.env.SLAB_LOOPBOY_BUS || join(
  process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab"),
  "loopboy",
  "bus",
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

function safeContact(value) {
  const contact = String(value || "").trim().toLowerCase();
  if (!/^[a-z0-9_-]{1,40}$/.test(contact)) {
    throw new Error("Loopboy bus requires a valid contact key");
  }
  return contact;
}

function contactDir(contact) {
  return join(LOOPBOY_BUS_ROOT, safeContact(contact));
}

export async function enqueueLoopboyEvent(event) {
  const sessionId = safeSessionId(event?.sessionId);
  const contact = safeContact(event?.contact);
  // The durable bus is contact-addressed, not route-addressed. A route is a
  // repairable delivery lease; replacing it must not strand already-queued
  // client messages in the prior session's directory.
  const dir = contactDir(contact);
  await mkdir(dir, { recursive: true, mode: 0o700 });
  await chmod(dir, 0o700);
  const createdAt = event.createdAt || new Date().toISOString();
  const id = event.id || randomUUID();
  const payload = {
    version: 1,
    id,
    sessionId,
    contact,
    channel: String(event.channel || "prox"),
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

async function claimOldest(dir, validate) {
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
      validate(event);
      return event;
    } finally {
      await unlink(claimed).catch(() => {});
    }
  }
  return null;
}

export async function waitLoopboyEvent(sessionId, {
  contact,
  timeoutMs = 50_000,
  pollMs = 200,
} = {}) {
  const sid = safeSessionId(sessionId);
  const contactKey = safeContact(contact);
  const deadline = Date.now() + Math.max(0, Math.min(55_000, Number(timeoutMs) || 0));
  do {
    const event = await claimOldest(contactDir(contactKey), (candidate) => {
      if (safeContact(candidate.contact) !== contactKey) {
        throw new Error("Loopboy bus event/contact mismatch");
      }
    });
    if (event) return event;
    // Drain the pre-bus session inbox as a compatibility migration. New
    // writers never use it, but an event queued just before an upgrade should
    // still be delivered exactly once after the listener comes back.
    const legacy = await claimOldest(sessionDir(sid), (candidate) => {
      if (candidate.sessionId !== sid) {
        throw new Error("Loopboy inbox event/session mismatch");
      }
      if (safeContact(candidate.contact) !== contactKey) {
        throw new Error("Loopboy inbox event/contact mismatch");
      }
    });
    if (legacy) return legacy;
    if (Date.now() >= deadline) return null;
    await sleep(Math.max(50, Math.min(1000, Number(pollMs) || 200)));
  } while (true);
}
