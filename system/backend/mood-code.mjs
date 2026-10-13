import { generateUniqueCode } from "./generate-short-code.mjs";

// A mood keeps its identity through handle/text changes. The original record
// ID remains the thread key so existing comments and saved posts still work.
const indexes = new WeakMap();
export async function ensureMoodCode(db, id) {
  const codes = db.collection("mood-codes");
  if (!indexes.has(db)) {
    const ready = codes.createIndex({ code: 1 }, { unique: true });
    indexes.set(db, ready);
    ready.catch(() => indexes.delete(db));
  }
  await indexes.get(db);
  const _id = String(id);
  for (let attempt = 0; attempt < 10; attempt++) {
    const existing = await codes.findOne({ _id });
    if (existing) return existing.code;
    const code = await generateUniqueCode(codes);
    try {
      await codes.insertOne({ _id, code });
      return code;
    } catch (error) {
      if (error.code !== 11000) throw error;
      // Another request assigned this mood, or claimed the same short code.
    }
  }
  throw new Error("Could not assign mood code");
}

export async function moodIdForCode(db, code) {
  return (await db.collection("mood-codes").findOne({ code }))?._id;
}
