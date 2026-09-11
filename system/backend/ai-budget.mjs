// ai-budget — what a handle has spent on inference today, and whether it may
// keep spending it on the good models.
//
// Aesthetic Computer pays for `/api/ask`. Until the gate landed, it paid for
// anyone; now it pays for anyone anonymously at the cheap tier, and for handles
// at whatever tier they ask for. That second promise is the one that needs a
// number attached to it, because `hint` is caller-supplied and a signed-in user
// asking for an expensive model in a loop is indistinguishable from a signed-in
// user working hard.
//
// The budget is a day bucket rather than a running balance. A month-long pool
// can be drained in an afternoon and then the tool is dead for three weeks,
// which is the worst shape this can take for someone who opened the editor to
// make something. A day resets, so the bad outcome is "come back tomorrow" and
// the worst case for AC is bounded at roughly thirty times a day's spend.
//
// Running out is not a wall. It drops the caller to the same cheap tier an
// anonymous visitor gets — `make` and `paint` keep working, they just stop
// costing what the good models cost. Refusing outright would turn a budget into
// an outage, and the point of the free tier is that the thing works.

import { connect } from "./database.mjs";

const COLLECTION = "ai-usage";

// Tokens per handle per UTC day. Sized against the models a handle can actually
// reach: ~25k tokens of gpt-4o is on the order of ten cents, so a day's ceiling
// is a few dollars a month per active handle rather than a few tens. It is a
// starting number chosen before there is real usage data to choose from, which
// is why it is an environment variable and not a constant in a branch.
export const DAILY_TOKEN_BUDGET = Number(process.env.AI_DAILY_TOKEN_BUDGET) || 25_000;

// The UTC day a usage document belongs to. UTC rather than local time so a
// handle does not get two budgets by flying east, and so the bucket a request
// lands in never depends on which server answered it.
export function dayKey(now = new Date()) {
  return now.toISOString().slice(0, 10);
}

function docId(handle, day) {
  return `${handle}:${day}`;
}

// What this handle has spent today. Returns `exhausted` rather than a decision:
// the caller owns what running out means, and it is not the same answer for
// every endpoint.
export async function checkBudget(handle, { now = new Date() } = {}) {
  const budget = DAILY_TOKEN_BUDGET;
  if (!handle) return { handle: "", used: 0, budget, exhausted: false, remaining: budget };
  const day = dayKey(now);
  let database;
  try {
    database = await connect();
    const doc = await database.db
      .collection(COLLECTION)
      .findOne({ _id: docId(handle, day) }, { projection: { tokens: 1 } });
    const used = Number(doc?.tokens) || 0;
    return {
      handle,
      day,
      used,
      budget,
      remaining: Math.max(0, budget - used),
      exhausted: used >= budget,
    };
  } catch (error) {
    // A budget lookup that fails must not take the endpoint down with it. An
    // unknown balance is treated as unspent: over-serving during a database
    // blip is recoverable, refusing everyone is not.
    console.log("🪙 budget lookup failed, assuming unspent —", error.message);
    return { handle, used: 0, budget, remaining: budget, exhausted: false, unknown: true };
  }
  finally {
    try { await database?.disconnect(); } catch {}
  }
}

// Add what a request actually cost. Called after the stream ends, with the
// counts the providers report rather than an estimate — both paths already
// parse them to draw their token bars, so the real number was always there.
//
// Never throws. A request that succeeded must not be reported as failed because
// the accounting write lost a race, and a dropped increment costs AC a few
// tenths of a cent while a thrown error costs the user their answer.
export async function recordUsage(handle, tokens, { model = "", now = new Date() } = {}) {
  const amount = Math.max(0, Math.round(Number(tokens) || 0));
  if (!handle || !amount) return null;
  const day = dayKey(now);
  let database;
  try {
    database = await connect();
    const result = await database.db.collection(COLLECTION).findOneAndUpdate(
      { _id: docId(handle, day) },
      {
        $inc: { tokens: amount, asks: 1 },
        $set: { last: now, lastModel: String(model || "") },
        $setOnInsert: { handle, day, first: now },
      },
      { upsert: true, returnDocument: "after" },
    );
    const updated = result?.value || result;
    return Number(updated?.tokens) || amount;
  } catch (error) {
    console.log("🪙 usage write failed —", error.message);
    return null;
  } finally {
    try { await database?.disconnect(); } catch {}
  }
}
