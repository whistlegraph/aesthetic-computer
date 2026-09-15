import { once } from "node:events";

// Preserve streaming through the Express adapter, including slow/disconnected
// clients. Header flush exposes connection establishment before token one.
export async function sendStream(res, body) {
  const reader = body.getReader();
  const cancelled = new AbortController();
  const close = () => {
    cancelled.abort();
    reader.cancel("client disconnected").catch(() => {});
  };
  res.once("close", close);
  res.socket?.setNoDelay(true);
  res.flushHeaders();
  try {
    while (!res.destroyed) {
      const { done, value } = await reader.read();
      if (done) break;
      if (!res.write(value)) await once(res, "drain", { signal: cancelled.signal });
      res.flush?.();
    }
    if (!res.destroyed) res.end();
  } finally {
    res.off("close", close);
    await reader.cancel().catch(() => {});
    reader.releaseLock();
  }
}
