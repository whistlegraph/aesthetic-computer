import { generateUniqueCode } from "./generate-short-code.mjs";

// A chat message keeps its identity through handle/text changes. The original record
// ID remains the thread key so existing comments and saved posts still work.
const indexes = new WeakMap();
export async function ensureChatCode(db, instance, id) {
  if (!["system", "clock"].includes(instance)) throw new Error("Unsupported public chat");
  const codes = db.collection("chat-codes");
  if (!indexes.has(db)) {
    const ready = codes.createIndex({ code: 1 }, { unique: true });
    indexes.set(db, ready);
    ready.catch(() => indexes.delete(db));
  }
  await indexes.get(db);
  const _id = instance + ":" + String(id);
  for (let attempt = 0; attempt < 10; attempt++) {
    const existing = await codes.findOne({ _id });
    if (existing) return existing.code;
    const code = await generateUniqueCode(codes);
    try {
      await codes.insertOne({ _id, code });
      return code;
    } catch (error) {
      if (error.code !== 11000) throw error;
      // Another request assigned this message, or claimed the same short code.
    }
  }
  throw new Error("Could not assign chat code");
}

export async function chatIdForCode(db, code) {
  const key = (await db.collection("chat-codes").findOne({ code }))?._id;
  const match = /^(system|clock):([a-f0-9]{24})$/.exec(key || "");
  return match ? { instance: match[1], id: match[2] } : null;
}
