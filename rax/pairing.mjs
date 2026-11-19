// 📱 Signing a screen in from a phone. A tab that is looking on asks its
// socket for a code and shows it as a QR; a phone already signed in to AC
// opens the page the QR names and, on a tap, posts the code with its own
// access token. The token is verified here and the waiting socket is told
// who it now is. Nothing but the handle goes to the tab: the token stays on
// this side, and nothing is minted that outlives the socket.
//
// A code is 128 random bits, lives two minutes, works once, and is kept in
// memory only, with the socket that asked for it. Codes and tokens are never
// logged.
import { randomBytes } from 'node:crypto';
import { pairNumber, CODE } from './pair-number.mjs';

// At most `max` in any `windowMs`, per key (an IP, a socket).
function limiter(max, windowMs, now) {
  const seen = new Map();
  return (key) => {
    const at = now();
    if (seen.size > 4096) for (const [k, v] of seen) if (at - v.at >= windowMs) seen.delete(k);
    if (seen.size > 8192) return false; // a flood of new keys: nobody new for a moment
    let v = seen.get(key);
    if (!v || at - v.at >= windowMs) seen.set(key, (v = { at, n: 0 }));
    return ++v.n <= max;
  };
}

export const PAIR_LIMITS = {
  ttl: 120000,
  live: 2, // codes a socket holds at once: the one shown and the one before it
  createPerSocket: [12, 600000],
  createPerIp: [40, 600000],
  approvePerIp: [10, 60000],
  approvePerSocket: [20, 600000],
  whoPerIp: [20, 60000],
  fails: 5, // refused approvals before a code is thrown away
  codes: 4096,
};

export function createPairing({ verify, handleOf = (a) => a?.handle || null, now = Date.now, limits = {} } = {}) {
  const L = { ...PAIR_LIMITS, ...limits };
  const codes = new Map(); // code → { socket, approve, until, fails }
  const made = limiter(...L.createPerSocket, now), madeFrom = limiter(...L.createPerIp, now);
  const tried = limiter(...L.approvePerIp, now), triedOn = limiter(...L.approvePerSocket, now);
  const asked = limiter(...L.whoPerIp, now);
  const sweep = () => { for (const [code, e] of codes) if (e.until <= now()) codes.delete(code); };
  const live = (code) => {
    const e = codes.get(code);
    if (e && e.until <= now()) codes.delete(code);
    return codes.get(code) || null;
  };
  const drop = (socket) => { for (const [code, e] of codes) if (e.socket === socket) codes.delete(code); };
  return {
    get size() { return codes.size; },
    drop,
    // A new code for this socket. `approve(account)` is the socket's own
    // sign-in: it answers whether the socket took it.
    async create(socket, ip, approve) {
      if (!made(socket) || !madeFrom(ip || 'unknown')) return { error: 'slow' };
      sweep();
      if (codes.size >= L.codes) return { error: 'busy' };
      const mine = [...codes].filter(([, e]) => e.socket === socket);
      for (const [code] of mine.slice(0, Math.max(0, mine.length - L.live + 1))) codes.delete(code);
      const code = randomBytes(16).toString('base64url');
      codes.set(code, { socket, approve, until: now() + L.ttl, fails: 0 });
      return { code, number: await pairNumber(code), ttl: L.ttl };
    },
    // The phone asks whose token it holds, to name the handle before the tap.
    async who(token, ip) {
      if (!asked(ip || 'unknown')) return { status: 429, error: 'slow' };
      if (typeof token !== 'string' || !token || token.length > 7500) return { status: 400, error: 'bad' };
      let account = null;
      try { account = await verify(token); } catch {}
      const handle = account && handleOf(account);
      return handle ? { status: 200, ok: true, handle } : account ? { status: 403, error: 'handle' } : { status: 401, error: 'account' };
    },
    // The phone's word: this code, and who I am. → { status, ok, handle, error }
    async approve(code, token, ip) {
      if (!tried(ip || 'unknown')) return { status: 429, error: 'slow' };
      if (typeof code !== 'string' || !CODE.test(code) || typeof token !== 'string' || !token || token.length > 7500)
        return { status: 400, error: 'bad' };
      const entry = live(code);
      if (!entry) return { status: 410, error: 'gone' };
      if (!triedOn(entry.socket)) return { status: 429, error: 'slow' };
      let account = null;
      try { account = await verify(token); } catch {}
      const handle = account && handleOf(account);
      if (!handle) {
        if (++entry.fails >= L.fails) codes.delete(code);
        return account ? { status: 403, error: 'handle' } : { status: 401, error: 'account' };
      }
      // Someone else may have used it while this token was being checked.
      if (live(code) !== entry) return { status: 410, error: 'gone' };
      drop(entry.socket); // used: this code and any other the socket held
      if (!entry.approve(account)) return { status: 410, error: 'gone' };
      return { status: 200, ok: true, handle };
    },
  };
}

// Who is asking, behind Caddy and the edge: their word for it is taken only
// when the connection itself comes from this machine.
export function ipOf(req) {
  const peer = req.socket?.remoteAddress || '';
  if (!/^(::1|127\.|::ffff:127\.)/.test(peer)) return peer;
  const told = req.headers['cf-connecting-ip'] || String(req.headers['x-forwarded-for'] || '').split(',').pop().trim();
  return (typeof told === 'string' && /^[0-9a-f:.]{3,45}$/i.test(told) && told) || peer;
}

// A small JSON body, or null.
export async function jsonBody(req, max = 9000) {
  let size = 0;
  const parts = [];
  try {
    for await (const part of req) {
      if ((size += part.length) > max) return null;
      parts.push(part);
    }
    return JSON.parse(Buffer.concat(parts).toString());
  } catch {
    return null;
  }
}
