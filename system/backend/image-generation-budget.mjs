// Reserve before inference; failed/ambiguous attempts keep their reservation.
// Cloudflare FLUX Schnell, 1024 square / four steps: $0.0006336, rounded up.
export const IMAGE_COST_MICRO_USD = 634;

function providerCost(provider) {
  if (provider === "cloudflare") return IMAGE_COST_MICRO_USD;
  // fal Sana at 768x768 remains within its one-megapixel price tier.
  if (provider === "fal-sana") return 1000;
  throw new Error("Unknown image generation provider");
}

function cap(value, fallback) {
  const dollars = Number(value ?? fallback);
  if (!Number.isFinite(dollars) || dollars < 0 || dollars > 1000)
    throw new Error("Invalid image generation budget");
  return Math.floor(dollars * 1_000_000);
}

export function createMongoImageBudget(collection, { env = process.env, now = Date.now } = {}) {
  return async function reserve({ provider = "cloudflare" } = {}) {
    const cost = providerCost(provider);
    const at = new Date(now());
    const month = at.toISOString().slice(0, 7);
    const day = at.toISOString().slice(0, 10);
    const monthly = cap(env.IMAGE_MONTHLY_BUDGET_USD, 5);
    const daily = cap(env.IMAGE_DAILY_BUDGET_USD, 0.5);
    // Keep the historical Cloudflare key: changing providers must not reset
    // spend already reserved this month. Daily/monthly totals include both.
    const id = `cloudflare-flux:${month}`;
    const field = `days.${day}`;
    const dayEnd = Date.UTC(at.getUTCFullYear(), at.getUTCMonth(), at.getUTCDate() + 1);
    const monthEnd = Date.UTC(at.getUTCFullYear(), at.getUTCMonth() + 1, 1);
    const denied = (end) => ({ allowed: false, retryAfterSeconds: Math.max(1, Math.ceil((end - at.getTime()) / 1000)) });
    if (monthly < cost) return denied(monthEnd);
    if (daily < cost) return denied(dayEnd);
    try {
      await collection.updateOne({ _id: id }, { $setOnInsert: {
        reservedMicroUsd: 0, attempts: 0, days: {}, createdAt: at,
      } }, { upsert: true });
    } catch (error) {
      if (error.code !== 11000) throw error;
    }
    // A single conditional document update enforces both limits across workers.
    const updated = await collection.findOneAndUpdate({
      _id: id,
      reservedMicroUsd: { $lte: monthly - cost },
      $or: [{ [field]: { $exists: false } }, { [field]: { $lte: daily - cost } }],
    }, {
      $inc: { reservedMicroUsd: cost, [field]: cost, attempts: 1 },
      $set: { updatedAt: at },
    }, { returnDocument: "after", includeResultMetadata: false });
    if ((updated?.value ?? updated)?.reservedMicroUsd) return { allowed: true };
    const current = await collection.findOne({ _id: id }, { projection: { reservedMicroUsd: 1 } });
    if (!current) throw new Error("Image budget record unavailable");
    return denied(current.reservedMicroUsd > monthly - cost ? monthEnd : dayEnd);
  };
}

export async function reserveImageBudget({ provider = "cloudflare" } = {}) {
  providerCost(provider); // Reject unknown providers before opening the database.
  const { connect } = await import("./database.mjs");
  const { db } = await connect();
  return createMongoImageBudget(db.collection("image-generation-budget"))({ provider });
}
