// braincell-book-backfill.mjs — open the Braincell book for wallets that
// predate it. Each wallet gets best-known entries for its grants, refunds,
// open holds and lifetime spend (marked `reconstructed`), and one opening
// `adjust` so its book sums to its balance.
//
// Run on Lith, after the book code is deployed:
//   cd /opt/ac/system && node --env-file=.env backend/migrations/braincell-book-backfill.mjs            # dry run
//   cd /opt/ac/system && node --env-file=.env backend/migrations/braincell-book-backfill.mjs --apply    # write
//   ... [--user <sub>] [--verbose] [--no-stripe]
//
// Stripe grants get their real time from session.created, fetched from
// Stripe (read-only, about four a second) unless --no-stripe; a lookup that
// fails leaves that grant at the wallet's creation time, marked atKnown:false.
// Live sessions use STRIPE_API_PRIV_KEY and test sessions
// STRIPE_API_TEST_PRIV_KEY, as easel-checkout does.
//
// Idempotent: an opened wallet carries ledgerOpenedAt and is skipped. The
// opening is one update guarded by the wallet's balance and ledgerSeq, so a
// wallet that moves while it is being read is simply read again.
import { connect, closePool } from "../database.mjs";
import { CREDIT_PACK } from "../easel-paid-credits.mjs";
import { BOOK_COLLECTION, bookBalance, drainOutbox, openingEntries, staged, stripeSessionTimes } from "../braincell-book.mjs";

const args = process.argv.slice(2);
const APPLY = args.includes("--apply"), VERBOSE = args.includes("--verbose");
const only = args.includes("--user") ? args[args.indexOf("--user") + 1] : null;
const STRIPE = !args.includes("--no-stripe");

// Session time by id, from whichever Stripe account the session lives in.
async function stripeTimes(ids) {
  if (!STRIPE || !ids.length) return { times: {}, failed: [], skipped: ids.length };
  if (!process.env.STRIPE_API_PRIV_KEY && !process.env.STRIPE_API_TEST_PRIV_KEY) return { times: {}, failed: ids };
  const { default: Stripe } = await import("stripe");
  const clients = {};
  const client = id => {
    const key = id.startsWith("cs_test_") ? process.env.STRIPE_API_TEST_PRIV_KEY : process.env.STRIPE_API_PRIV_KEY;
    if (!key) throw Error("no Stripe key");
    return clients[key] ??= new Stripe(key, { timeout: 20000, maxNetworkRetries: 2 });
  };
  return stripeSessionTimes(ids, id => client(id).checkout.sessions.retrieve(id));
}

const { db } = await connect();
try {
  const wallets = db.collection("ac-credit-wallets"), entries = db.collection(BOOK_COLLECTION);

  // What each source recorded about its grants, by grant id.
  const purchases = {}, tezos = {};
  for (const p of await db.collection("ac-credit-purchases").find({}, { projection: { credits: 1, environment: 1, redeemedAt: 1, createdAt: 1 } }).toArray())
    purchases[p._id] = { credits: p.credits, environment: p.environment, at: p.redeemedAt || p.createdAt };
  for (const p of await db.collection("whistlegraph-iap-purchases").find({}, { projection: { credits: 1, environment: 1, deliveredAt: 1, createdAt: 1 } }).toArray())
    purchases[p._id] = { credits: p.credits, environment: p.environment, at: p.deliveredAt || p.createdAt };
  for (const t of await db.collection("ac-tezos-checkouts").find({ grantId: { $exists: true } }, { projection: { grantId: 1, credits: 1, operationHash: 1, creditedAt: 1 } }).toArray())
    tezos[t.grantId] = { credits: t.credits, operationHash: t.operationHash, at: t.creditedAt };

  const unopened = await wallets.find({ ...(only ? { _id: only } : {}), ledgerOpenedAt: { $exists: false } }, { projection: { grants: 1 } }).toArray();
  const sessions = [...new Set(unopened.flatMap(w => (w.grants || []).map(String)).filter(id => id.startsWith("cs_")))];
  const { times: stripe, failed: stripeFailed } = await stripeTimes(sessions);

  const totals = { wallets: 0, alreadyOpen: 0, opened: 0, entries: 0, unknownGrants: 0, adjust: 0, raced: 0,
    stripeSessions: sessions.length, stripeTimed: Object.keys(stripe).length, stripeFailed: STRIPE ? stripeFailed.length : "skipped" };
  const bySource = {};
  const ids = await wallets.find(only ? { _id: only } : {}, { projection: { _id: 1 } }).toArray();
  for (const { _id } of ids) {
    totals.wallets++;
    for (let attempt = 0; ; attempt++) {
      // Wallet first, then its rows (see bookBalance).
      const wallet = await wallets.findOne({ _id });
      if (!wallet) break; // deleted while we read
      if (wallet.ledgerOpenedAt) { totals.alreadyOpen++; break; }
      const rows = await entries.find({ user: _id }).toArray();
      const now = new Date();
      const opening = openingEntries(wallet, rows, { packCredits: CREDIT_PACK.credits, purchases, tezos, stripe, now });
      const book = bookBalance({ ...wallet, outbox: [...(wallet.outbox || []), ...opening] }, rows).total;
      if (book !== (Number(wallet.balance) || 0)) throw Error(`opening for ${_id} does not sum: book ${book}, balance ${wallet.balance}`);
      if (VERBOSE || only) console.log(JSON.stringify({ user: _id, balance: wallet.balance, opening }, null, 1));
      if (APPLY) {
        const guard = { _id, balance: wallet.balance, ledgerOpenedAt: { $exists: false },
          ledgerSeq: wallet.ledgerSeq ?? { $exists: false } };
        const result = await wallets.updateOne(guard, staged({ $set: { ledgerOpenedAt: now } }, opening, now));
        if (result.modifiedCount !== 1) {
          if (attempt >= 5) throw Error(`wallet ${_id} kept changing; run again`);
          totals.raced++; continue;
        }
      }
      totals.opened++; totals.entries += opening.length;
      for (const e of opening) {
        if (e.amountKnown === false) totals.unknownGrants++;
        if (e.kind === "adjust") totals.adjust += e.amount;
        const key = `${e.kind}:${e.source}`;
        (bySource[key] ??= { entries: 0, braincells: 0 }).entries++;
        bySource[key].braincells += e.amount;
      }
      break;
    }
  }
  if (APPLY) totals.drained = await drainOutbox(wallets, entries, { limit: 100_000 });
  console.log(JSON.stringify({ mode: APPLY ? "apply" : "dry run", ...totals, bySource }, null, 2));
} finally { await closePool(); }
