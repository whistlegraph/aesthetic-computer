// fetch-retry.mjs — fetch that survives a flaky link.
//
// On a bad network (neo, 2026-10-06: SYNs dropped, ~3 s connects, the odd
// ECONNRESET mid-handshake) undici gives up on the first reset and all a tool
// can say is "fetch failed". Only failures that never reached the server are
// retried — an HTTP status, even a 5xx, comes back as is — so this is safe for
// reads. Don't wrap a POST that must not land twice.

const BACKOFF_MS = [500, 1500, 3000, 6000];

export async function fetchRetry(url, init = {}) {
  for (let attempt = 0; ; attempt++) {
    try {
      return await fetch(url, init);
    } catch (err) {
      // An abort from the caller's own signal is a timeout, not a flake.
      if (err.name === "AbortError" || err.name === "TimeoutError" || attempt >= BACKOFF_MS.length) {
        const cause = err.cause?.code || err.cause?.message;
        throw new Error(`${err.message}${cause ? ` (${cause})` : ""} after ${attempt + 1} tries: ${new URL(url).host}`);
      }
      await new Promise((r) => setTimeout(r, BACKOFF_MS[attempt]));
    }
  }
}
