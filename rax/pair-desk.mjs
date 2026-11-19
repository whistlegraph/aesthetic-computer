// 📱 The tab's side of a pairing (pairing.mjs): while it looks on with nobody
// signed in it keeps one live code from its socket, asked for again before
// the code runs out, and forgets it with the socket. A code lives in this
// page's memory only: a reload asks for a new one.
export const PAIR_URL = 'https://aesthetic.computer/rax/pair#';
const SETTLE = 1500; // a walker between rooms looks gated for a moment: wait it out
const EARLY = 30000; // a code shown has at least this long left to be scanned and approved

export function createPairDesk() {
  let socket = null, since = 0, asked = 0, pair = null;
  return {
    // What the room said back: a code, or that it is asking too fast.
    hear(ws, data, now = performance.now()) {
      if (ws !== socket || typeof data.code !== 'string' || !/^\d{4}$/.test(data.number || '')) return;
      pair = { url: PAIR_URL + data.code, number: data.number, until: now + (data.ttl || 0) };
    },
    // Called every frame: what to draw (or null), asking the socket as needed.
    want(ws, wanted, now = performance.now()) {
      if (!wanted || ws?.readyState !== 1) return (socket = pair = null);
      if (ws !== socket) { socket = ws; since = now; asked = 0; pair = null; }
      if (pair && pair.until <= now) pair = null;
      const due = !pair || pair.until - now < EARLY;
      if (due && now - since > SETTLE && (!asked || now - asked > 20000)) {
        asked = now;
        ws.send(JSON.stringify({ type: 'pair' }));
      }
      return pair;
    },
  };
}
