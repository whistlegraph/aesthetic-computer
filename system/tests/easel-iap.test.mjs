import test from "node:test";
import assert from "node:assert/strict";
import { createHandler, grantFor } from "../netlify/functions/easel-iap.mjs";

const transaction = { transactionId: "2000000123", originalTransactionId: "2000000123", bundleId: "computer.aesthetic.easel",
  productId: "computer.aesthetic.easel.braincells.1m", type: "Consumable", quantity: 1, purchaseDate: 1789750000000 };
const verifier = (environment, payloads) => ({ environment,
  verifyAndDecodeTransaction: async jws => { if (!payloads[jws]) throw new Error("bad"); return payloads[jws]; },
  verifyAndDecodeNotification: async jws => { if (!payloads[jws]) throw new Error("bad"); return payloads[jws]; } });
function wallet() {
  const doc = { balance: 0, grants: [], refunds: {} };
  const wallets = {
    async updateOne(filter, update) {
      if (Array.isArray(update)) { // revoke pipeline
        const id = Object.keys(update[0].$set).find(k => k.startsWith("refunds."))?.slice(8);
        const credits = update[0].$set["refunds." + id].$max[0];
        const already = doc.refunds[id] || 0;
        doc.balance -= Math.max(0, credits - already); doc.refunds[id] = Math.max(credits, already);
        return { modifiedCount: 1 };
      }
      if (update.$setOnInsert) return { matchedCount: 1 };
      if (filter.grants?.$ne && doc.grants.includes(filter.grants.$ne)) return { modifiedCount: 0 };
      doc.balance += update.$inc.balance; doc.grants.push(update.$addToSet.grants);
      return { modifiedCount: 1 };
    },
  };
  return { doc, wallets };
}
const post = (body, auth = "Bearer t") => ({ httpMethod: "POST", headers: auth ? { authorization: auth } : {}, body: JSON.stringify(body) });

test("grantFor accepts only this app's consumable", () => {
  assert.equal(grantFor(transaction, "u").id, "apple:2000000123");
  assert.equal(grantFor({ ...transaction, productId: "other" }, "u").error, "Unknown product");
  assert.equal(grantFor({ ...transaction, bundleId: "x" }, "u").error, "Wrong app");
  assert.equal(grantFor({ ...transaction, revocationDate: 1 }, "u").error, "Refunded");
  assert.equal(grantFor({ ...transaction, quantity: 3 }, "u").credits, 3_000_000);
});

test("a verified purchase credits once, and a retry reports it already credited", async () => {
  const { doc, wallets } = wallet();
  const ledger = [];
  const handler = createHandler({ verifiers: [verifier("Production", { good: transaction })],
    verifyUser: async () => ({ sub: "user" }), wallets: fn => fn(wallets), purchases: async d => { ledger.push(d); } });
  let result = await handler(post({ jws: "good" }));
  assert.equal(result.statusCode, 200);
  assert.deepEqual(JSON.parse(result.body), { credited: true, transactionId: "2000000123", credits: 1_000_000, environment: "Production" });
  assert.equal(doc.balance, 1_000_000);
  result = await handler(post({ jws: "good" }));
  assert.equal(JSON.parse(result.body).credited, false);
  assert.equal(doc.balance, 1_000_000);
  assert.equal(ledger[0].user, "user");
});

test("an unverifiable or unsigned-in purchase grants nothing", async () => {
  const { doc, wallets } = wallet();
  const handler = createHandler({ verifiers: [verifier("Production", { good: transaction })],
    verifyUser: async h => { if (!h.authorization) throw new Error("no"); return { sub: "user" }; }, wallets: fn => fn(wallets) });
  assert.equal((await handler(post({ jws: "forged" }))).statusCode, 401);
  assert.equal((await handler(post({ jws: "good" }, null))).statusCode, 401);
  assert.equal(doc.balance, 0);
});

test("sandbox transactions only count when a sandbox verifier is configured", async () => {
  const { doc, wallets } = wallet();
  const handler = createHandler({ verifiers: [verifier("Production", {})], verifyUser: async () => ({ sub: "user" }), wallets: fn => fn(wallets) });
  assert.equal((await handler(post({ jws: "sandbox" }))).statusCode, 401);
  assert.equal(doc.balance, 0);
});

test("an Apple refund notification takes the pack back from its owner", async () => {
  const { doc, wallets } = wallet();
  const owners = { "apple:2000000123": { user: "user" } };
  const handler = createHandler({
    verifiers: [verifier("Production", { good: transaction, refund: { notificationType: "REFUND", notificationUUID: "n1", data: { signedTransactionInfo: "good" } } })],
    verifyUser: async () => ({ sub: "user" }), wallets: fn => fn(wallets),
    purchases: async d => d.find ? owners[d.find] : undefined });
  await handler(post({ jws: "good" }));
  assert.equal(doc.balance, 1_000_000);
  const result = await handler({ httpMethod: "POST", headers: {}, body: JSON.stringify({ signedPayload: "refund" }) });
  assert.equal(result.statusCode, 200);
  assert.equal(doc.balance, 0);
  await handler({ httpMethod: "POST", headers: {}, body: JSON.stringify({ signedPayload: "refund" }) });
  assert.equal(doc.balance, 0, "a redelivered refund converges");
});
