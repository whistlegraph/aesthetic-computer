import { test } from "node:test";
import assert from "node:assert/strict";
import { foldTezosRevenue, revenueSource } from "../../toolchain/analytics/tezos-revenue.mjs";
import { TEZOS, KEEPS } from "../../toolchain/mcp/tezos-wallets.mjs";

test("payments into AC's wallets fold by source, wallet, sender and buyer; internal moves are left out", () => {
  const report = foldTezosRevenue([
    { amount: 4_000_000, sender: { address: "KT1market", alias: "objkt.com Issuer contract" }, initiator: { address: "tz1buyer", alias: "zaphodok" }, target: { address: TEZOS.aesthetic } },
    { amount: 1_000_000, sender: { address: "tz1fan" }, target: { address: KEEPS } },
    { amount: 2_500_000, sender: { address: "tz1someone" }, target: { address: TEZOS.aesthetic } },
    { amount: 9_000_000, sender: { address: TEZOS.keeps }, target: { address: TEZOS.aesthetic } },
  ], { usdPerTez: 0.3 });
  assert.equal(report.xtz, 7.5);
  assert.equal(report.payments, 3);
  assert.equal(report.usd, 2.25);
  assert.deepEqual(report.bySource.map(r => r.key), ["objkt sales and royalties",
    "direct to aesthetic.tez (includes Braincell tez checkouts)", "keeps mint fees"]);
  assert.deepEqual(report.byWallet.map(r => [r.key, r.xtz]), [["aesthetic.tez", 6.5], ["Keeps contract", 1]]);
  assert.deepEqual(report.topBuyers, [{ key: "zaphodok", xtz: 4, payments: 1, usd: 1.2 }]);
});

test("teia sales are named apart from objkt", () => {
  assert.equal(revenueSource({ sender: { alias: "Teia Marketplace" }, target: { address: TEZOS.kidlisp } }), "teia / hic et nunc sales");
  assert.equal(revenueSource({ sender: { alias: "" }, target: { address: TEZOS.kidlisp } }), "direct payments");
});
