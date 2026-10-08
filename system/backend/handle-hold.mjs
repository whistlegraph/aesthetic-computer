// handle-hold, 26.10.08
// A newcomer picks their @handle before they have an account, so the name has
// to be kept for them while they go and fetch an email code. A hold is ten
// minutes on one anonymous attempt id (the signup funnel's UUID): nobody else
// can claim the name in that window, and `/handle` honours it.
//
//   GET  /api/handle-hold?handle=x[&attempt=uuid] → { status: free|taken|held|yours|invalid, reason? }
//   POST /api/handle-hold { handle, attempt }      → { held: true, until }
//
// Holds live in `handle-holds` keyed by the lowercased handle, with a TTL
// index, so an abandoned attempt frees the name on its own.

import { respond } from "./http.mjs";

export const HOLDS = "handle-holds";
export const HOLD_MS = 10 * 60 * 1000;
const attemptID = (value) => typeof value === "string" && /^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}$/i.test(value);
const exactly = (handle) => new RegExp(`^${handle.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}$`, "i");

// True when someone else's live hold covers this handle. `/handle` calls this
// before it creates or renames, passing the attempt id the claimant brought.
export async function heldByOther(db, handle, attempt, now = new Date()) {
  const hold = await db.collection(HOLDS).findOne({ _id: String(handle).toLowerCase(), until: { $gt: now } });
  return !!hold && hold.attempt !== attempt;
}

// Called after a successful claim so the record does not linger.
export async function releaseHold(db, handle) {
  await db.collection(HOLDS).deleteOne({ _id: String(handle).toLowerCase() });
}

export function createHandleHoldHandler({ connect, validateHandle, filter, handleQuarantined, now = () => new Date() }) {
  const rates = new Map();
  let indexed = false;

  async function status(db, handle, attempt) {
    const valid = validateHandle(handle);
    if (valid !== "valid") return { status: "invalid", reason: valid };
    if (filter(handle) !== handle) return { status: "invalid", reason: "naughty" };
    if (await db.collection("@handles").findOne({ handle: exactly(handle) }, { projection: { _id: 1 } })) return { status: "taken" };
    if (await handleQuarantined(db, handle, now())) return { status: "taken" };
    const hold = await db.collection(HOLDS).findOne({ _id: handle.toLowerCase(), until: { $gt: now() } });
    if (hold) return { status: hold.attempt === attempt ? "yours" : "held" };
    return { status: "free" };
  }

  return async (event) => {
    const reply = (code, body) => respond(code, body, { "Cache-Control": "no-store" });
    if (event.httpMethod === "OPTIONS") return reply(204, "");
    if (!["GET", "POST"].includes(event.httpMethod)) return reply(405, { message: "Method not allowed" });

    // A live availability check fires on every pause in typing; keep a
    // generous ceiling per address so it stays a check, not an enumerator.
    const ip = String(event.headers?.["x-forwarded-for"] || event.headers?.["x-real-ip"] || "").split(",")[0].trim() || "?";
    const t = Date.now();
    for (const [key, rate] of rates) if (rate.until <= t) rates.delete(key);
    const rate = rates.get(ip) || { until: t + 60000, count: 0 };
    rates.set(ip, rate);
    if (++rate.count > 90) return reply(429, { message: "Slow down a little" });

    let handle, attempt;
    if (event.httpMethod === "GET") {
      handle = event.queryStringParameters?.handle;
      attempt = event.queryStringParameters?.attempt;
    } else {
      try { ({ handle, attempt } = JSON.parse(event.body || "{}")); } catch { return reply(400, { message: "Invalid JSON" }); }
      if (!attemptID(attempt)) return reply(400, { message: "Invalid attempt" });
    }
    handle = String(handle || "").trim().replace(/^@/, "");
    if (!handle || handle.length > 32) return reply(400, { status: "invalid", reason: "empty" });

    let database;
    try {
      database = await connect();
      const { db } = database;
      const current = await status(db, handle, attempt);
      if (event.httpMethod === "GET") return reply(200, current);
      if (current.status !== "free" && current.status !== "yours") return reply(409, current);

      const holds = db.collection(HOLDS);
      if (!indexed) { await holds.createIndex({ until: 1 }, { expireAfterSeconds: 0 }); indexed = true; }
      const until = new Date(+now() + HOLD_MS);
      // One name per attempt: changing your mind releases the last one.
      await holds.deleteMany({ attempt, _id: { $ne: handle.toLowerCase() } });
      try {
        await holds.updateOne(
          { _id: handle.toLowerCase(), $or: [{ until: { $lte: now() } }, { attempt }] },
          { $set: { attempt, handle, until } },
          { upsert: true },
        );
      } catch (error) {
        if (error?.code === 11000) return reply(409, { status: "held" }); // raced: someone else's live hold
        throw error;
      }
      return reply(200, { held: true, until: until.toISOString() });
    } catch {
      return reply(503, { message: "Handle check unavailable" });
    } finally {
      await database?.disconnect?.();
    }
  };
}
