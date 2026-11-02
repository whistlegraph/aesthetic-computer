// braincell-book — the book of record for Braincells. Every change to a
// wallet's balance writes one immutable entry: what kind of change, where the
// Braincells came from or went, how many, and the outside reference that
// proves it. braincell-spends stays the per-request reporting row; this is
// the money, and it has no TTL.
//
// Atomicity: an entry is staged on the wallet document (wallet.outbox) by the
// same single-document update that moves the balance, so the two cannot
// drift on any Mongo topology, and a caller's transaction (No Paint,
// Whistlegraph speech) carries the entry with it for free. drainOutbox copies
// staged entries into braincell-entries under deterministic ids, then pulls
// them; a crash between the two leaves the entry staged and the next drain
// finishes it. Anything that reads the book takes the union of the
// collection and the outbox, by id.
//
// No prompt, output, token or address is ever written: entries are built
// from an allowlist.
import { createHash } from "node:crypto";

export const BOOK_COLLECTION = "braincell-entries";
export const KINDS = Object.freeze(["issue", "spend", "refund", "revoke", "reserve", "release", "adjust"]);
// Where Braincells came from (stripe, apple, tezos, staff), the hosted
// inference that holds and spends them, and the one-time opening of the book.
export const SOURCES = Object.freeze(["stripe", "apple", "tezos", "staff", "inference", "backfill"]);
const SIGN = { issue: 1, release: 1, spend: -1, refund: -1, revoke: -1, reserve: -1, adjust: 0 };
const REF_FIELDS = ["stripeSession", "appleTransaction", "appleEnvironment", "tezosPayment", "tezosOperation", "issuer", "reason"];

export const grantSource = id => id.startsWith("cs_") ? "stripe" : id.startsWith("apple:") ? "apple" : id.startsWith("tezos:") ? "tezos" : "staff";

// The outside reference a grant id already carries.
export function grantRef(id) {
  const source = grantSource(id);
  if (source === "stripe") return { stripeSession: id };
  if (source === "apple") return { appleTransaction: id.split(":").pop() };
  if (source === "tezos") return { tezosPayment: id };
  return {};
}

// Same wallet and key, same id: a retried write can never book twice.
export const entryId = (user, key) => createHash("sha256").update(`${user}\0${key}`).digest("hex").slice(0, 32);

const clip = (value, max = 200) => String(value).slice(0, max);

export function bookEntry({ user, key, kind, source, amount, at = new Date(), balanceAfter, product, model,
  grantId, holdId, ref, reconstructed, amountKnown, atKnown, note }) {
  if (!user || typeof key !== "string" || !key) throw Error("A book entry needs a wallet and an idempotency key");
  if (!KINDS.includes(kind)) throw Error(`Unknown book entry kind: ${kind}`);
  if (!SOURCES.includes(source)) throw Error(`Unknown book entry source: ${source}`);
  if (!Number.isSafeInteger(amount)) throw Error("A book entry moves a whole number of Braincells");
  // A reconstructed grant whose size nothing recorded books 0; the opening
  // adjust carries its Braincells instead.
  const unknown = reconstructed && amountKnown === false;
  if (unknown ? amount !== 0 : amount === 0 || (SIGN[kind] && Math.sign(amount) !== SIGN[kind]))
    throw Error(`A ${kind} entry cannot move ${amount}`);
  const entry = { _id: entryId(user, key), key: clip(key, 300), kind, source, amount, at: new Date(at) };
  if (Number.isSafeInteger(balanceAfter)) entry.balanceAfter = balanceAfter;
  if (product) entry.product = clip(product, 40);
  if (model) entry.model = clip(model, 120);
  if (grantId) entry.grantId = clip(grantId);
  if (holdId) entry.holdId = clip(holdId, 80);
  const kept = Object.fromEntries(REF_FIELDS.filter(k => ref?.[k] != null && ref[k] !== "").map(k => [k, clip(ref[k])]));
  if (Object.keys(kept).length) entry.ref = kept;
  if (reconstructed) entry.reconstructed = true;
  if (unknown) entry.amountKnown = false;
  if (typeof atKnown === "boolean") entry.atKnown = atKnown;
  if (note) entry.note = clip(note, 300);
  return entry;
}

// Fold entries into a wallet update: staged in order, counted, flagged for
// the drain. ledgerSeq rises by exactly the entries staged, which is what
// lets the drain number them.
export function staged(update, entries, now = new Date()) {
  if (!entries.length) return update;
  return { ...update,
    $push: { ...update.$push, outbox: { $each: entries } },
    $inc: { ...update.$inc, ledgerSeq: entries.length },
    $set: { ...update.$set, outboxAt: now } };
}

// Staged entries as collection rows. The outbox is always the newest
// contiguous run of the wallet's entries, so its last one is ledgerSeq.
export function bookRows(wallet) {
  const pending = wallet.outbox || [], last = Number(wallet.ledgerSeq) || pending.length;
  return pending.map((entry, i) => ({ ...entry, user: wallet._id, seq: last - pending.length + i + 1 }));
}

let indexed = false;
export async function ensureBookIndexes(entries, wallets) {
  await entries.createIndex({ user: 1, seq: 1 }, { name: "book_by_wallet" });
  await entries.createIndex({ at: 1, kind: 1, source: 1 }, { name: "book_by_time" });
  await entries.createIndex({ grantId: 1 }, { name: "book_by_grant", sparse: true });
  await wallets.createIndex({ outboxAt: 1 }, { name: "book_outbox", sparse: true });
}

// Move staged entries into the collection. Safe to run anywhere, any number
// of times at once: inserts are keyed, pulls name only what was copied.
export async function drainOutbox(wallets, entries, { user, limit = 200 } = {}) {
  if (!indexed) { indexed = true; await ensureBookIndexes(entries, wallets).catch(() => { indexed = false; }); }
  let moved = 0;
  const pending = await wallets.find({ ...(user ? { _id: user } : {}), outboxAt: { $exists: true } },
    { projection: { outbox: 1, ledgerSeq: 1 } }).limit(limit).toArray();
  for (const wallet of pending) {
    const rows = bookRows(wallet);
    for (const row of rows) {
      try { await entries.insertOne(row); moved++; }
      catch (error) { if (error.code !== 11000) throw error; }
    }
    if (rows.length) await wallets.updateOne({ _id: wallet._id }, { $pull: { outbox: { _id: { $in: rows.map(r => r._id) } } } });
    await wallets.updateOne({ _id: wallet._id, $or: [{ outbox: { $size: 0 } }, { outbox: { $exists: false } }] },
      { $unset: { outbox: "", outboxAt: "" } });
  }
  return moved;
}

// 🧮 The book's balance for one wallet: drained rows and staged entries,
// each counted once. Read the wallet first, then its rows: an entry drained
// in between is then in the rows, never in neither.
export function bookBalance(wallet, rows) {
  const all = new Map(rows.map(r => [r._id, r]));
  for (const entry of wallet.outbox || []) all.set(entry._id, entry);
  let total = 0;
  for (const entry of all.values()) total += Number(entry.amount) || 0;
  return { total, entries: all.size, staged: (wallet.outbox || []).length, all: [...all.values()] };
}

export function walletDrift(wallet, rows) {
  const { total, entries, staged: pending } = bookBalance(wallet, rows);
  const balance = Number(wallet.balance) || 0;
  return { user: wallet._id, balance, book: total, drift: balance - total, entries, staged: pending,
    opened: Boolean(wallet.ledgerOpenedAt) };
}

const sum = list => list.reduce((n, e) => n + (Number(e.amount) || 0), 0);

// What a grant was worth and when, from the records each source keeps.
// purchases: grant id → { credits, at, environment } (ac-credit-purchases,
// whistlegraph-iap-purchases). tezos: grant id → { credits, at, operationHash }
// (ac-tezos-checkouts). stripe: session id → { at } (session.created, fetched
// from Stripe by the backfill). Stripe sold one fixed pack; paidCheckout
// refused any other amount, so its size is known even when its time is not.
export function recoverIssue(id, { packCredits, purchases = {}, tezos = {}, stripe = {} } = {}) {
  const source = grantSource(id), ref = grantRef(id);
  if (source === "stripe") return stripe[id]?.at
    ? { source, amount: packCredits, ref, at: stripe[id].at, atKnown: true }
    : { source, amount: packCredits, ref, note: "Stripe pack; time not recorded locally" };
  if (source === "apple") {
    const p = purchases[id];
    if (Number.isSafeInteger(p?.credits) && p.credits > 0)
      return { source, amount: p.credits, at: p.at, ref: { ...ref, appleEnvironment: p.environment } };
    // Whistlegraph sells exactly one pack per purchase.
    if (id.startsWith("apple:whistlegraph:")) return { source, amount: packCredits, ref: { ...ref, appleEnvironment: id.split(":")[2] }, note: "one Whistlegraph pack; time not recorded" };
    return { source, amount: null, ref, note: "no purchase record; quantity unknown" };
  }
  if (source === "tezos") {
    const t = tezos[id];
    if (Number.isSafeInteger(t?.credits) && t.credits > 0) return { source, amount: t.credits, at: t.at, ref: { ...ref, tezosOperation: t.operationHash } };
    return { source, amount: null, ref, note: "no Tezos checkout record" };
  }
  return { source, amount: null, ref, note: "staff grant; amount was never recorded" };
}

// When each Stripe checkout happened, for the opening: one retrieve per
// session, spaced out so the backfill stays well under Stripe's rate limit.
// A failed lookup is skipped; that grant keeps the wallet's time, unknown.
export async function stripeSessionTimes(ids, retrieve, { gapMs = 250, sleep = ms => new Promise(r => setTimeout(r, ms)) } = {}) {
  const times = {}, failed = [];
  for (const [i, id] of ids.entries()) {
    if (i) await sleep(gapMs);
    try {
      const session = await retrieve(id);
      if (session?.id === id && Number.isSafeInteger(session.created)) times[id] = { at: new Date(session.created * 1000) };
      else failed.push(id);
    } catch { failed.push(id); }
  }
  return { times, failed };
}

// The one-time opening of a wallet's book: best-known entries for history
// that predates it, then one adjust so the book sums to the balance. Every
// key matches the key the live code would have used (issue:<grant>,
// reserve:<hold>), so a grant or hold already booked live is never doubled.
export function openingEntries(wallet, rows, { packCredits, purchases = {}, tezos = {}, stripe = {}, now = new Date() } = {}) {
  const user = wallet._id, opened = wallet.createdAt ? new Date(wallet.createdAt) : now;
  const known = new Map(bookBalance(wallet, rows).all.map(e => [e._id, e]));
  const has = key => known.has(entryId(user, key));
  const out = [];
  const add = fields => {
    const entry = bookEntry({ user, reconstructed: true, ...fields });
    if (!known.has(entry._id)) { out.push(entry); known.set(entry._id, entry); }
  };
  const book = () => [...known.values()];

  for (const id of (wallet.grants || []).map(String)) {
    if (has(`issue:${id}`)) continue;
    const found = recoverIssue(id, { packCredits, purchases, tezos, stripe });
    add({ key: `issue:${id}`, kind: "issue", source: found.source, grantId: id, ref: found.ref, note: found.note,
      amount: found.amount ?? 0, ...(found.amount == null ? { amountKnown: false } : {}),
      at: found.at || opened, atKnown: found.at ? found.atKnown : false });
  }
  // Refunds are cumulative per grant; book whatever the live entries lack.
  // A grant's adjust entries are Apple reversals, which give refunds back.
  for (const [id, refunded] of Object.entries(wallet.refunds || {})) {
    const source = grantSource(id), kind = source === "stripe" ? "refund" : "revoke";
    const taken = -sum(book().filter(e => e.grantId === id && ["refund", "revoke", "adjust"].includes(e.kind)));
    const missing = (Number(refunded) || 0) - taken;
    if (missing > 0) add({ key: `${kind}:${id}:opening`, kind, source, grantId: id, amount: -missing, ref: grantRef(id), at: opened, atKnown: false });
  }
  // Holds reserved before the book: the open ones, and the ones settled live
  // whose release has no reserve to pair with.
  for (const [holdId, hold] of Object.entries(wallet.holds || {}))
    if (Number(hold.amount) > 0) add({ key: `reserve:${holdId}`, kind: "reserve", source: "inference", holdId,
      amount: -hold.amount, product: hold.product, model: hold.model, at: hold.at || opened });
  for (const release of book().filter(e => e.kind === "release" && e.holdId))
    add({ key: `reserve:${release.holdId}`, kind: "reserve", source: "inference", holdId: release.holdId,
      amount: -release.amount, at: release.at, atKnown: false, note: "reserved before the book opened" });
  // Lifetime spend the live entries do not account for.
  const unbooked = (Number(wallet.spent) || 0) + sum(book().filter(e => e.kind === "spend"));
  if (unbooked > 0) add({ key: "spend:opening", kind: "spend", source: "inference", amount: -unbooked, at: opened, atKnown: false,
    note: "lifetime spend before the book opened; per-request detail is in braincell-spends" });

  const unknown = out.filter(e => e.amountKnown === false).length;
  const gap = (Number(wallet.balance) || 0) - sum(book());
  if (gap) add({ key: "adjust:opening", kind: "adjust", source: "backfill", amount: gap, at: now, balanceAfter: Number(wallet.balance) || 0,
    note: `opening balance: what the reconstructed entries do not explain${unknown ? ` (${unknown} grant${unknown > 1 ? "s" : ""} of unknown size)` : ""}` });
  return out;
}
