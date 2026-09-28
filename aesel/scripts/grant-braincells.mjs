// grant-braincells.mjs — top up a handle's bought braincells by hand.
//
// Staff use, on Lith with its environment (the Mongo connection lith.service
// runs with). The grant goes through the same ledger a purchase does, under a
// grant id of its own, so running the same command twice with the same --id
// adds nothing the second time.
//
//   node aesel/scripts/grant-braincells.mjs @jeffrey 5000000 [--id staff-2026-09-28]
import { randomUUID } from "node:crypto";
import { connect } from "../../system/backend/database.mjs";
import { balance, fulfillGrant, withWallets } from "../../system/backend/easel-paid-credits.mjs";

const args = process.argv.slice(2);
const flag = (name) => { const i = args.indexOf(name); return i >= 0 ? args.splice(i, 2)[1] : undefined; };
const id = flag("--id") || `staff-${new Date().toISOString().slice(0, 10)}-${randomUUID().slice(0, 8)}`;
const [who = "", amount = ""] = args;
const handle = who.replace(/^@/, "").toLowerCase();
const credits = Number(amount);
if (!handle || !Number.isSafeInteger(credits) || credits < 1) {
  console.error("usage: node aesel/scripts/grant-braincells.mjs @handle <braincells> [--id grant-id]");
  process.exit(1);
}

const connection = await connect();
let user;
try { user = (await connection.db.collection("@handles").findOne({ handle }, { projection: { _id: 1 } }))?._id; }
finally { await connection.disconnect(); }
if (!user) { console.error(`no such handle: @${handle}`); process.exit(1); }

const result = await withWallets(async (wallets) => {
  const granted = await fulfillGrant({ user, id, credits }, wallets);
  return { granted, balance: await balance(user, wallets) };
});
console.log(JSON.stringify({ handle: `@${handle}`, id, credits, granted: result.granted, balance: result.balance }));
// The database module keeps its pool open; the grant is done, so leave.
process.exit(0);
