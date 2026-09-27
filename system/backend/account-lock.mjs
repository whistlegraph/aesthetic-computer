// Account lock, 2026.09.26
// An account waiting to be deleted is locked: authorize() refuses its
// tokens even while Auth0 would still accept them. Kept apart from
// account-deletion.mjs so authorization.mjs can import it cheaply.
//
// A lock is a Redis key that expires on its own: through the grace period
// and any retries while the account exists, then for a day after deletion
// so access tokens issued before the lock cannot outlive it.

import * as KeyValue from "./kv.mjs";

const DAY = 24 * 60 * 60;
const key = (sub) => `account-lock:${sub}`;
const LOOKUP_MS = 250;

// Fails open when Redis is slow or down: the Auth0 block still stops new
// sign-ins, and waiting on Redis would stall every signed-in request.
export async function accountLocked(sub) {
  if (!sub) return false;
  let timer;
  const timeout = new Promise((resolve) => {
    timer = setTimeout(() => resolve(false), LOOKUP_MS);
    timer.unref?.();
  });
  const lookup = (async () => {
    await KeyValue.connect();
    return !!(await KeyValue.getKey(key(sub)));
  })().catch(() => false);
  try {
    return await Promise.race([lookup, timeout]);
  } finally {
    clearTimeout(timer);
  }
}

// Held until the purge finishes, with room for retries.
export async function setAccountLock(sub, until) {
  await KeyValue.connect();
  const seconds = (new Date(until).getTime() - Date.now()) / 1000 + 90 * DAY;
  await KeyValue.setExpiring(key(sub), new Date(until).toISOString(), seconds);
}

export async function clearAccountLock(sub) {
  await KeyValue.connect();
  await KeyValue.delKey(key(sub));
}

// After deletion: a day, longer than any access token lives.
export async function holdDeletedAccountLock(sub) {
  await KeyValue.connect();
  await KeyValue.setExpiring(key(sub), "deleted", DAY);
}
