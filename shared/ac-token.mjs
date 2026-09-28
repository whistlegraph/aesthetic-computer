// ac-token.mjs — the one way to renew ~/.ac-token.
//
// Auth0 rotates refresh tokens: each refresh spends the old one. Two
// processes on one Mac refreshing at once (ac-login, Menu Band, Aesel, an
// MCP) would spend it twice and sign that Mac out. So every refresher holds
// the `~/.ac-token.lock` directory while it refreshes, then re-reads the
// file: if someone else already renewed it, it keeps theirs.
//
// mkdir is the lock because it is atomic from Node and Swift alike
// (slab/menuband + shared/swift ACSession.swift take the same one). A lock
// older than 30 s belonged to a process that died and is taken over.
//
// Across machines there is nothing to lock: each Mac signs in with its own
// `ac-login` and gets its own refresh token. Never copy ~/.ac-token between
// machines — with rotation, the first refresh on one kills the other.

import { mkdir, readFile, rm, stat, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";

export const AUTH_DOMAIN = "hi.aesthetic.computer";
export const CLIENT_ID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
export const TOKEN_FILE = join(homedir(), ".ac-token");
const EARLY_MS = 60 * 1000; // Renew a minute before expiry.
const STALE_LOCK_MS = 30 * 1000;
const WAIT_MS = 20 * 1000;

export async function withTokenLock(work, file = TOKEN_FILE) {
  const lock = file + ".lock";
  const deadline = Date.now() + WAIT_MS;
  for (;;) {
    try { await mkdir(lock); break; }
    catch (error) {
      if (error.code !== "EEXIST") throw error;
      const age = Date.now() - ((await stat(lock).catch(() => null))?.mtimeMs ?? Date.now());
      if (age > STALE_LOCK_MS) { await rm(lock, { recursive: true, force: true }); continue; }
      if (Date.now() > deadline) throw new Error(`Another refresh is holding ${lock}`);
      await new Promise((resolve) => setTimeout(resolve, 200));
    }
  }
  try { return await work(); }
  finally { await rm(lock, { recursive: true, force: true }); }
}

// The session record with a usable access token, renewed through the refresh
// grant under the lock when it is within a minute of expiring (or `force`).
// Keeps every other field; writes in place so file watchers fire.
export async function freshSession({ file = TOKEN_FILE, force = false, fetch = globalThis.fetch } = {}) {
  const usable = (record) => !force && record?.access_token && !(record.expires_at && Date.now() > record.expires_at - EARLY_MS);
  const current = JSON.parse(await readFile(file, "utf8"));
  if (usable(current)) return current;
  return withTokenLock(async () => {
    const record = JSON.parse(await readFile(file, "utf8"));
    if (usable(record)) return record; // Renewed by another process while we waited.
    if (!record.refresh_token) throw new Error("No refresh token; run `ac-login`");
    const response = await fetch(`https://${AUTH_DOMAIN}/oauth/token`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ grant_type: "refresh_token", client_id: CLIENT_ID, refresh_token: record.refresh_token }),
    });
    if (!response.ok) throw new Error(`AC session refresh failed (${response.status}); run \`ac-login\``);
    const next = await response.json();
    record.access_token = next.access_token;
    if (next.refresh_token) record.refresh_token = next.refresh_token;
    if (next.id_token) record.id_token = next.id_token;
    record.expires_at = Date.now() + (next.expires_in || 3600) * 1000;
    await writeFile(file, `${JSON.stringify(record, null, 2)}\n`, { mode: 0o600 });
    return record;
  }, file);
}
