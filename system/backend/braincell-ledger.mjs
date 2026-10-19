// braincell-ledger — one row per settled hosted-inference request, so the
// network report can say which models Braincells went to and for whom.
//
// The wallet (ac-credit-wallets) already holds the money: balance, lifetime
// spent and spend per UTC day. It never held the model, and the free
// allowance (ai-usage) keeps only the last model of each day. This ledger is
// the per-request record those two lack. It is written after settlement and
// never decides anything, so a failed write costs a row of reporting, never a
// charge or an answer.
import { connect } from "./database.mjs";

export const LEDGER_COLLECTION = "braincell-spends";
// Long enough for year-over-year questions, short enough not to grow forever.
export const LEDGER_RETENTION_DAYS = 400;

const clean = (value, max = 120) => String(value ?? "").slice(0, max);

// A row carries no prompt, output, address or token: who, which model, how
// much, and whether purchased Braincells paid for it.
export function spendRow({ product, client, handle, user, model, braincells, paid, at = new Date() }) {
  const amount = Math.max(0, Math.ceil(Number(braincells) || 0));
  if (!amount) return null;
  return {
    at, day: at.toISOString().slice(0, 10),
    product: clean(product, 40) || "unknown", client: clean(client, 40),
    handle: clean(handle, 64), user: clean(user, 200), model: clean(model) || "unknown",
    braincells: amount, paid: Boolean(paid),
  };
}

let indexed = false;
export async function ensureLedgerIndexes(collection) {
  await collection.createIndex({ at: 1 }, { name: "spend_retention", expireAfterSeconds: LEDGER_RETENTION_DAYS * 86400 });
  await collection.createIndex({ handle: 1, at: -1 }, { name: "spend_by_handle" });
}

// Never throws: the request already succeeded and was already billed.
export async function recordSpend(spend, { db } = {}) {
  const row = spendRow(spend);
  if (!row) return false;
  let connection;
  try {
    if (!db) { connection = await connect(); db = connection.db; }
    const collection = db.collection(LEDGER_COLLECTION);
    if (!indexed) { indexed = true; await ensureLedgerIndexes(collection).catch(() => { indexed = false; }); }
    await collection.insertOne(row);
    return true;
  } catch (error) {
    console.log("🧠 braincell ledger write failed —", error.message);
    return false;
  } finally {
    try { await connection?.disconnect(); } catch {}
  }
}
