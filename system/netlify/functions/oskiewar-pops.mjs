// oskiewar-pops, 26.08.09
// Counts the one thing every visitor to oskiewar.com does: pop the dummy.
//
// The dummy fight runs live under the title screen, so this is the statistic
// the whole site already shares without anyone signing in. One row per pop,
// nothing in it but the hour it happened — no player, no match, no address —
// and a TTL index sweeps the rows out on their own. Counting a window is then
// a single aggregation over what is left.

import { createHmac } from "node:crypto";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const COLLECTION = "oskiewar-pops";
const HOUR = 60 * 60 * 1000;
// The longest window the statistic will ever quote, and so the longest a row
// is worth keeping. An hour of slack past it covers the sweep's own latency.
export const MAX_HOURS = 48;
const TTL_SECONDS = (MAX_HOURS + 1) * 60 * 60;
// Windows the line is allowed to say, shortest first. The shortest one that
// still has something in it is the one that gets quoted, so a busy afternoon
// reads as an hour's worth and a quiet night widens out to two days rather
// than reporting nothing.
export const WINDOWS = Object.freeze([1, 3, 6, 12, 24, MAX_HOURS]);
// One browser cannot run the number up on its own. A dummy takes a few seconds
// to kill, so this is far above honest play and far below a script.
const POPS_PER_HOUR = 400;

// Given how many pops fell in each hour-bucket back from now, pick the window
// to quote and the count to quote with it. Zero is a real answer: the caller
// is expected to say nothing rather than boast about none.
export function chooseWindow(byHour) {
  for (const hours of WINDOWS) {
    let pops = 0;
    for (let hour = 0; hour < hours; hour++) pops += byHour[hour] || 0;
    if (pops > 0) return { pops, hours };
  }
  return { pops: 0, hours: MAX_HOURS };
}

function sourceDigest(event) {
  const headers = event.headers || {};
  const ip = headers["cf-connecting-ip"] ||
    headers["x-forwarded-for"]?.split(",")[0]?.trim() || "unknown";
  const secret = process.env.REPLAY_HASH_SECRET || process.env.JWT_SECRET ||
    "ac-pops-rate-v1";
  return createHmac("sha256", secret).update(ip).digest("hex");
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");
  if (event.httpMethod !== "GET" && event.httpMethod !== "POST")
    return respond(405, { error: "Method not allowed" });

  let database;
  try {
    database = await connect();
    const collection = database.db.collection(COLLECTION);
    const now = Date.now();

    if (event.httpMethod === "POST") {
      const digest = sourceDigest(event);
      const recent = await collection.countDocuments({ sourceDigest: digest,
        at: { $gte: new Date(now - HOUR) } });
      if (recent >= POPS_PER_HOUR) {
        await database.disconnect();
        return respond(429, { error: "Pop rate limit reached" });
      }
      await collection.insertOne({ at: new Date(now), sourceDigest: digest });
      await database.disconnect();
      return respond(201, { ok: true });
    }

    // The TTL index is declared on the read path because it is idempotent and
    // this endpoint has no deploy step of its own to hang it off.
    await collection.createIndex({ at: 1 },
      { expireAfterSeconds: TTL_SECONDS }).catch(() => {});
    // `$subtract` will take one date from another and hand back milliseconds,
    // but only in that order — a bare number on the left is an error, and an
    // empty collection never evaluates the expression to tell you so.
    const rows = await collection.aggregate([
      { $match: { at: { $gte: new Date(now - MAX_HOURS * HOUR) } } },
      { $group: {
        _id: { $floor: { $divide: [{ $subtract: [new Date(now), "$at"] }, HOUR] } },
        pops: { $sum: 1 } } },
    ]).toArray();
    await database.disconnect();
    const byHour = [];
    for (const row of rows) byHour[Number(row._id)] = row.pops;
    const { pops, hours } = chooseWindow(byHour);
    return respond(200, { pops, hours, maxHours: MAX_HOURS }, {
      // Every visitor to the title screen asks for this, and a minute stale is
      // a fine price for not hitting the database once per page load.
      "Cache-Control": "public, max-age=60",
    });
  } catch (error) {
    if (database) await database.disconnect();
    return respond(500, { error: error.message || "Pop count failed" });
  }
}
