import { createHash, randomBytes } from "node:crypto";

const key = () => randomBytes(32).toString("base64url");
const digest = (value) => createHash("sha256").update(value).digest("hex");
const MAX_AGE = 5 * 60_000;

// The caller supplies a server-verified AC principal, never form fields.
// The local runner uses an explicitly labelled disposable test principal.
export class LinkFlow {
  constructor({ now = Date.now } = {}) {
    this.now = now;
    this.transactions = new Map();
    this.byDid = new Map();
    this.bySubject = new Map();
  }

  checkPrincipal(principal) {
    if (!principal?.subject || !principal.browser || principal.locked ||
        !Number.isFinite(principal.authenticatedAt) ||
        this.now() < principal.authenticatedAt ||
        this.now() - principal.authenticatedAt >= MAX_AGE) {
      throw new Error("A fresh, unlocked AC session is required.");
    }
  }

  begin(principal) {
    this.checkPrincipal(principal);
    for (const [id, tx] of this.transactions) {
      if (tx.expiresAt <= this.now() || tx.browser === digest(principal.browser)) {
        this.transactions.delete(id);
      }
    }
    if (this.transactions.size >= 100) throw new Error("Too many pending connections.");
    const id = key();
    this.transactions.set(id, {
      subject: principal.subject,
      browser: digest(principal.browser),
      expiresAt: this.now() + MAX_AGE,
    });
    return id;
  }

  pending(id, principal) {
    this.checkPrincipal(principal);
    const tx = this.transactions.get(id);
    if (!tx || tx.expiresAt <= this.now()) {
      this.transactions.delete(id);
      throw new Error("This connection expired or was already used.");
    }
    if (tx.subject !== principal.subject || tx.browser !== digest(principal.browser)) {
      throw new Error("Return in the same signed-in browser that started the connection.");
    }
    return { ...tx };
  }

  // did must come exclusively from NodeOAuthClient.callback().session.did.
  verify(id, principal, did) {
    this.pending(id, principal);
    if (typeof did !== "string" || !/^did:(plc:[a-z2-7]{24}|web:[a-zA-Z0-9.:%_-]+)$/.test(did)) {
      throw new Error("OAuth did not return a valid AT Protocol identity.");
    }
    this.transactions.get(id).did = did;
  }

  confirm(id, principal) {
    const tx = this.pending(id, principal);
    if (!tx.did) throw new Error("Complete Bluesky authorization first.");
    // One synchronous critical section in this single-process prototype.
    // Production needs unique database indexes plus an atomic write.
    this.transactions.delete(id);
    const owner = this.byDid.get(tx.did);
    const existing = this.bySubject.get(tx.subject);
    if (owner && owner !== tx.subject) throw new Error("This Bluesky identity is linked to another account.");
    if (existing && existing !== tx.did) throw new Error("Disconnect the existing Bluesky identity first.");
    this.byDid.set(tx.did, tx.subject);
    this.bySubject.set(tx.subject, tx.did);
    return tx.did;
  }

  disconnect(principal) {
    this.checkPrincipal(principal);
    const did = this.bySubject.get(principal.subject);
    if (did) this.byDid.delete(did);
    this.bySubject.delete(principal.subject);
    for (const [id, tx] of this.transactions) {
      if (tx.subject === principal.subject) this.transactions.delete(id);
    }
  }
}

export function memoryStore({ now = Date.now, ttl = 10 * 60_000 } = {}) {
  const values = new Map();
  return {
    async set(id, value) {
      for (const [k, entry] of values) if (entry.expiresAt <= now()) values.delete(k);
      if (!values.has(id) && values.size >= 100) throw new Error("OAuth store is full.");
      values.set(id, { value: structuredClone(value), expiresAt: now() + ttl });
    },
    async get(id) {
      const entry = values.get(id);
      if (!entry || entry.expiresAt <= now()) {
        values.delete(id);
        return undefined;
      }
      return structuredClone(entry.value);
    },
    async del(id) { values.delete(id); },
    async clear() { values.clear(); },
  };
}
