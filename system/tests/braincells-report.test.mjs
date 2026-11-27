import { test } from "node:test";
import assert from "node:assert/strict";
import { foldBraincells, windowDays, grantKind } from "../../toolchain/analytics/braincells-fold.mjs";
import { spendRow, recordSpend, LEDGER_COLLECTION } from "../backend/braincell-ledger.mjs";

const start = new Date("2026-10-18T18:00:00Z"), end = new Date("2026-10-19T18:00:00Z");

test("a window names every UTC day it touches", () => {
  assert.deepEqual(windowDays(start, end), ["2026-10-18", "2026-10-19"]);
  assert.deepEqual(windowDays(new Date("2026-10-19T00:00:00Z"), new Date("2026-10-19T06:00:00Z")), ["2026-10-19"]);
});

test("grant ids sort into who paid for them", () => {
  assert.equal(grantKind("cs_live_abc"), "stripe");
  assert.equal(grantKind("apple:200001"), "apple");
  assert.equal(grantKind("tezos:mainnet:42"), "tezos");
  assert.equal(grantKind("staff-2026-09-28"), "staff");
});

test("circulation, spending by model and handle, and purchases fold from raw records", () => {
  const report = foldBraincells({
    wallets: [
      { _id: "sub-a", balance: 1_000_000, spent: 400_000, grants: ["cs_live_1", "staff-x"], daily: { "2026-10-19": 200_000, "2026-10-01": 9 }, holds: { h: { amount: 5000 } } },
      { _id: "sub-b", balance: -2_000, spent: 10, grants: ["apple:1"] },
      { _id: "sub-c", balance: 0 },
    ],
    allowance: [{ handle: "bee", day: "2026-10-19", tokens: 150_000, asks: 3, lastModel: "deepseek/deepseek-v4.1-flash" },
      { handle: "kid", day: "2026-10-19", tokens: 0, nopaint: 25_000, asks: 5, lastModel: "tencent/hy-image-v3.5-preview" }],
    ledger: [
      { product: "aesel", handle: "ay", user: "sub-a", model: "anthropic/claude-sonnet-5.5", braincells: 200_000, paid: true },
      { product: "aesel", handle: "bee", user: "sub-b", model: "deepseek/deepseek-v4.1-flash", braincells: 150_000, paid: false },
    ],
    nopaint: [{ user: "sub-a", model: "google/gemini-image", charged: 40_000, paid: 40_000 }],
    speech: [{ user: "sub-b", charged: 120, paid: 0 }],
    apple: [{ user: "sub-b", credits: 1_000_000, refundedAt: null }],
    tezos: [], stripe: { available: false, reason: "test" },
    handles: { "sub-a": "@ay", "sub-b": "@bee" },
    openRouter: { balance_usd: 12.5 },
  }, { start, end });

  assert.deepEqual(report.circulation.held, { braincells: 1_000_000, usd: 5 });
  assert.equal(report.circulation.debt.braincells, 2_000);
  assert.equal(report.circulation.reservedInFlight.braincells, 5000);
  assert.equal(report.circulation.fundedWallets, 1);
  assert.deepEqual(report.circulation.grantsByKind, { stripe: 1, staff: 1, apple: 1 });
  assert.deepEqual(report.circulation.topHolders, [{ handle: "@ay", braincells: 1_000_000, usd: 5 }]);

  // Only the window's days count toward purchased spend.
  assert.equal(report.spending.totals.purchasedSpent.braincells, 200_000);
  assert.equal(report.spending.totals.freeAllowanceUsed.braincells, 175_000);
  assert.ok(report.spending.allowanceByHandle.some(r => r.handle === "@kid" && r.braincells === 25_000 && r.asks === 5));
  assert.equal(report.spending.byModel[0].model, "anthropic/claude-sonnet-5.5");
  assert.equal(report.spending.byModel[0].paidBraincells, 200_000);
  assert.ok(report.spending.byModel.some(r => r.model === "openai/whisper-1" && r.braincells === 120));
  assert.equal(report.spending.byHandle[0].handle, "@ay");
  assert.equal(report.spending.byHandle[0].braincells, 240_000);
  assert.deepEqual(report.spending.byProduct.map(r => r.product), ["aesel", "nopaint", "whistlegraph-speech"]);
  assert.equal(report.purchases.apple.braincells, 1_000_000);
  assert.deepEqual(report.purchases.apple.buyers, ["@bee"]);
  assert.equal(report.openRouter.balance_usd, 12.5);
});

test("a ledger row keeps who, which model and how much, and nothing else", () => {
  const at = new Date("2026-10-19T12:00:00Z");
  const row = spendRow({ product: "aesel", client: "node", handle: "ay", user: "sub-a", model: "m", braincells: 10.2, paid: 1, at, prompt: "secret" });
  assert.deepEqual(row, { at, day: "2026-10-19", product: "aesel", client: "node", handle: "ay", user: "sub-a", model: "m", braincells: 11, paid: true });
  assert.equal(spendRow({ braincells: 0 }), null);
});

test("recording a spend never throws, even when the database refuses", async () => {
  const rows = [];
  const db = { collection: name => ({ createIndex: async () => {}, insertOne: async row => { assert.equal(name, LEDGER_COLLECTION); rows.push(row); } }) };
  assert.equal(await recordSpend({ product: "aesel", model: "m", braincells: 5 }, { db }), true);
  assert.equal(rows.length, 1);
  const broken = { collection: () => ({ createIndex: async () => {}, insertOne: async () => { throw new Error("down"); } }) };
  assert.equal(await recordSpend({ product: "aesel", model: "m", braincells: 5 }, { db: broken }), false);
});

test("a ledger row keeps the provider's token counts, and the report says how much input the cache served", async () => {
  const { foldBraincells } = await import("../../toolchain/analytics/braincells-fold.mjs");
  const at = new Date("2026-11-27T12:00:00Z");
  const usage = { input_tokens: 1000, output_tokens: 200, cache_read_input_tokens: 8000, cache_creation_input_tokens: 1000, cost: 0.01 };
  const row = spendRow({ product: "aesel", handle: "ay", user: "sub-a", model: "m", braincells: 40, paid: true, usage, provider: "openrouter", at });
  assert.deepEqual(row.tokens, { input: 1000, output: 200, cacheRead: 8000, cacheWrite: 1000 });
  assert.equal(row.provider, "openrouter");
  assert.equal(spendRow({ braincells: 5, usage: {} }).tokens, undefined);
  const old = spendRow({ product: "aesel", handle: "ay", user: "sub-a", model: "m", braincells: 60, paid: true, at });
  const report = foldBraincells({ ledger: [row, old] }, { start: new Date("2026-11-27T00:00:00Z"), end: new Date("2026-11-28T00:00:00Z") });
  const m = report.spending.byModel[0];
  assert.equal(m.requests, 2);
  assert.equal(m.meteredRequests, 1);
  assert.equal(m.cacheReadTokens, 8000);
  assert.equal(m.cacheReadShare, 0.8);
});
