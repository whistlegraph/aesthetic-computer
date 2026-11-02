#!/usr/bin/env node
// Run on Lith: cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/braincells-reconcile.mjs [--user <sub>] [--all]
// Sum each wallet's Braincell book against its balance and report drift.
// Read-only. A wallet the backfill has not opened yet cannot balance, so it
// is counted, not listed, unless --all. Exits 1 when an opened wallet drifts.
import { connect, closePool } from "../../system/backend/database.mjs";
import { BOOK_COLLECTION, walletDrift } from "../../system/backend/braincell-book.mjs";

const args = process.argv.slice(2);
const only = args.includes("--user") ? args[args.indexOf("--user") + 1] : null;
const all = args.includes("--all");

const { db } = await connect();
let drifting = 0;
try {
  const wallets = db.collection("ac-credit-wallets"), entries = db.collection(BOOK_COLLECTION);
  const ids = await wallets.find(only ? { _id: only } : {}, { projection: { _id: 1 } }).toArray();
  const handles = Object.fromEntries((await db.collection("@handles").find({ _id: { $in: ids.map(w => w._id) } }, { projection: { handle: 1 } }).toArray())
    .map(h => [h._id, `@${h.handle}`]));
  const report = { wallets: ids.length, opened: 0, unopened: 0, balanced: 0, staged: 0, drift: [] };
  let oldestStaged = null;
  for (const { _id } of ids) {
    // Wallet first, then its rows; a wallet that moved in between is read
    // again rather than reported as drift that is not there.
    let check;
    for (let attempt = 0; attempt < 3; attempt++) {
      const wallet = await wallets.findOne({ _id });
      if (!wallet) break;
      check = walletDrift(wallet, await entries.find({ user: _id }, { projection: { amount: 1 } }).toArray());
      if (wallet.outboxAt && (!oldestStaged || wallet.outboxAt < oldestStaged)) oldestStaged = wallet.outboxAt;
      const again = await wallets.findOne({ _id }, { projection: { balance: 1, ledgerSeq: 1 } });
      if (again?.balance === wallet.balance && again?.ledgerSeq === wallet.ledgerSeq) break;
    }
    if (!check) continue;
    report.staged += check.staged;
    check.opened ? report.opened++ : report.unopened++;
    if (!check.drift) { report.balanced++; continue; }
    if (check.opened) drifting++;
    if (check.opened || all) report.drift.push({ ...check, handle: handles[_id] || null });
  }
  report.drift.sort((a, b) => Math.abs(b.drift) - Math.abs(a.drift));
  console.log(JSON.stringify({ ...report, oldestStaged }, null, 2));
} finally { await closePool(); }
process.exitCode = drifting ? 1 : 0;
