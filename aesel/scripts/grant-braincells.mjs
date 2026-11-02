// grant-braincells.mjs — top up a handle's bought braincells by hand.
//
// Staff use, on Lith with its environment (the Mongo connection lith.service
// runs with). The grant goes through the same ledger a purchase does, under a
// grant id of its own, so running the same command twice with the same --id
// adds nothing the second time. The Braincell book records who issued it and
// why, so both --by (an existing AC handle) and --reason are required.
//
//   node aesel/scripts/grant-braincells.mjs @jeffrey 5000000 --by @jeffrey --reason "launch thank-you" [--id staff-2026-09-28]
import { randomUUID } from "node:crypto";
import { connect } from "../../system/backend/database.mjs";
import { balance, fulfillGrant, withWallets } from "../../system/backend/easel-paid-credits.mjs";
import { validateHandle } from "../../system/public/aesthetic.computer/lib/text.mjs";

const usage = "usage: node aesel/scripts/grant-braincells.mjs @handle <braincells> --by @issuer --reason <text> [--id grant-id]";
const args = process.argv.slice(2);
const flag = (name) => { const i = args.indexOf(name); return i >= 0 ? args.splice(i, 2)[1] : undefined; };
const id = flag("--id") || `staff-${new Date().toISOString().slice(0, 10)}-${randomUUID().slice(0, 8)}`;
const by = (flag("--by") || "").trim(), reason = (flag("--reason") || "").trim();
const [who = "", amount = ""] = args;
const handle = who.replace(/^@/, "").toLowerCase();
const issuerHandle = by.replace(/^@/, "").toLowerCase();
const credits = Number(amount);
const stop = (message) => { console.error(message); process.exit(1); };
if (!handle || !Number.isSafeInteger(credits) || credits < 1) stop(usage);
if (!by.startsWith("@") || ["invalid", "too long"].includes(validateHandle(issuerHandle))) stop(`--by needs the issuer's @handle\n${usage}`);
if (!reason) stop(`--reason is required\n${usage}`);

const connection = await connect();
let user, issuer;
try {
  const handles = connection.db.collection("@handles");
  user = (await handles.findOne({ handle }, { projection: { _id: 1 } }))?._id;
  issuer = (await handles.findOne({ handle: issuerHandle }, { projection: { _id: 1 } }))?._id;
} finally { await connection.disconnect(); }
if (!user) stop(`no such handle: @${handle}`);
if (!issuer) stop(`no such issuer handle: @${issuerHandle}`);

const result = await withWallets(async (wallets) => {
  const granted = await fulfillGrant({ user, id, credits, source: "staff", ref: { issuer: `@${issuerHandle}`, reason } }, wallets);
  return { granted, balance: await balance(user, wallets) };
});
console.log(JSON.stringify({ handle: `@${handle}`, id, credits, issuer: `@${issuerHandle}`, reason, granted: result.granted, balance: result.balance }));
// The database module keeps its pool open; the grant is done, so leave.
process.exit(0);
