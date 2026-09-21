import test from "node:test";
import assert from "node:assert/strict";
import { createMongoImageBudget } from "../backend/image-generation-budget.mjs";

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
