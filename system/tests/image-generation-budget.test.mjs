import test from "node:test";
import assert from "node:assert/strict";
import { createMongoImageBudget, reserveImageBudget } from "../backend/image-generation-budget.mjs";

const now = () => Date.parse("2026-09-21T12:00:00Z");
const noDatabase = new Proxy({}, { get() { throw new Error("Unexpected database access"); } });

test("zero budgets disable inference without a database request", async () => {
  assert.deepEqual(await createMongoImageBudget(noDatabase, {
    now, env: { IMAGE_MONTHLY_BUDGET_USD: "0" },
  })(), { allowed: false, retryAfterSeconds: 820800 });
  assert.deepEqual(await createMongoImageBudget(noDatabase, {
    now, env: { IMAGE_DAILY_BUDGET_USD: "0" },
  })(), { allowed: false, retryAfterSeconds: 43200 });
});

test("invalid budget configuration fails closed", async () => {
  for (const value of ["NaN", "Infinity", "-1", "1001"]) {
    await assert.rejects(createMongoImageBudget(noDatabase, {
      now, env: { IMAGE_MONTHLY_BUDGET_USD: value },
    }), /Invalid image generation budget/);
  }
});

test("database failure cannot grant a reservation", async () => {
  await assert.rejects(createMongoImageBudget({
    async updateOne() { throw new Error("synthetic outage"); },
  }, { now, env: {} }), /synthetic outage/);
});

test("exhausted reservations distinguish daily and monthly reset times", async () => {
  for (const [reservedMicroUsd, retryAfterSeconds] of [[500000, 43200], [4999900, 820800]]) {
    const reserve = createMongoImageBudget({
      async updateOne() {},
      async findOneAndUpdate() { return null; },
      async findOne() { return { reservedMicroUsd }; },
    }, { now, env: {} });
    assert.deepEqual(await reserve(), { allowed: false, retryAfterSeconds });
  }
});


test("unknown providers fail before any database access", async () => {
  const reserve = createMongoImageBudget(noDatabase, { now, env: {} });
  for (const provider of ["nvidia", "__proto__", "constructor", null, {}]) {
    await assert.rejects(reserve({ provider }), /Unknown image generation provider/);
    await assert.rejects(reserveImageBudget({ provider }), /Unknown image generation provider/);
  }
});

// A small in-memory conditional ledger for mixed-provider behavior; production
// Mongo atomicity is independently exercised using a temporary collection.
function ledgerFixture(seed) {
  const documents = new Map(seed ? [[seed._id, structuredClone(seed)]] : []);
  return {
    documents,
    async updateOne({ _id }, { $setOnInsert }) {
      if (!documents.has(_id)) documents.set(_id, { _id, ...structuredClone($setOnInsert) });
    },
    async findOneAndUpdate(query, { $inc, $set }) {
      const document = documents.get(query._id);
      const dayField = Object.keys($inc).find((field) => field.startsWith("days."));
      const day = dayField.slice(5);
      const dailyLimit = query.$or[1][dayField].$lte;
      if (document.reservedMicroUsd > query.reservedMicroUsd.$lte ||
          (document.days[day] ?? 0) > dailyLimit) return null;
      document.reservedMicroUsd += $inc.reservedMicroUsd;
      document.days[day] = (document.days[day] ?? 0) + $inc[dayField];
      document.attempts += $inc.attempts;
      Object.assign(document, $set);
      return structuredClone(document);
    },
    async findOne({ _id }) { return documents.get(_id); },
  };
}

test("mixed providers share exact daily and monthly caps across recreated reservers", async () => {
  let at = now();
  const collection = ledgerFixture();
  const options = {
    now: () => at,
    env: { IMAGE_DAILY_BUDGET_USD: "0.002268", IMAGE_MONTHLY_BUDGET_USD: "0.003268" },
  };
  const reserve = createMongoImageBudget(collection, options);
  assert.deepEqual(await reserve(), { allowed: true }); // Default remains Cloudflare.
  assert.deepEqual(await reserve({ provider: "fal-sana" }), { allowed: true });
  const restarted = createMongoImageBudget(collection, options);
  assert.deepEqual(await restarted({ provider: "cloudflare" }), { allowed: true });
  assert.deepEqual(await restarted({ provider: "fal-sana" }), {
    allowed: false, retryAfterSeconds: 43200,
  });
  const document = collection.documents.get("cloudflare-flux:2026-09");
  assert.equal(collection.documents.size, 1);
  assert.equal(document.reservedMicroUsd, 2268);
  assert.equal(document.days["2026-09-21"], 2268);
  assert.equal(document.attempts, 3);
  at += 86400000;
  assert.deepEqual(await restarted({ provider: "fal-sana" }), { allowed: true });
  assert.deepEqual(await restarted({ provider: "cloudflare" }), {
    allowed: false, retryAfterSeconds: 734400,
  });
  assert.equal(document.reservedMicroUsd, 3268);
  assert.equal(document.days["2026-09-22"], 1000);
  assert.equal(document.attempts, 4);
});

test("Sana reservations retain spend from the historical Cloudflare ledger", async () => {
  const collection = ledgerFixture({
    _id: "cloudflare-flux:2026-09", reservedMicroUsd: 634, attempts: 1,
    days: { "2026-09-21": 634 },
  });
  const reserve = createMongoImageBudget(collection, {
    now,
    env: { IMAGE_DAILY_BUDGET_USD: "0.001634", IMAGE_MONTHLY_BUDGET_USD: "0.001634" },
  });
  assert.deepEqual(await reserve({ provider: "fal-sana" }), { allowed: true });
  assert.equal((await reserve({ provider: "cloudflare" })).allowed, false);
  assert.equal((await reserve({ provider: "fal-sana" })).allowed, false);
  assert.equal(collection.documents.size, 1);
  const document = collection.documents.get("cloudflare-flux:2026-09");
  assert.equal(document.reservedMicroUsd, 1634);
  assert.equal(document.attempts, 2);
});
