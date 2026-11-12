// laklok-recent, 2026.11.12
// Who was last in laklok, and when: the handles whose signed-in tab most
// recently opened the room or sat visible in it, newest first. Read from the
// account-activity ledger (laklok_visible_minute ticks and piece_opened), so a
// reader who never posts still counts, and nothing rests on a client's say-so.

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { LAKLOK_PIECES, LAKLOK_FEATURE_VERSION, LAKLOK_PRESENCE } from "../../public/aesthetic.computer/lib/laklok-activity.mjs";

const WINDOW_MS = 14 * 24 * 60 * 60 * 1000;
const LIMIT = 8;

export function createLaklokRecentHandler(deps = {}) {
  const openDatabase = deps.connect || connect;
  const now = deps.now || (() => Date.now());
  return async function handler(event) {
    if (event.httpMethod === "OPTIONS") return respond(204, "");
    if (event.httpMethod !== "GET") return respond(405, { message: "GET only" });
    let database;
    try {
      database = await openDatabase();
      const { db } = database;
      const rows = await db.collection("account-activity").aggregate([
        { $match: {
          at: { $gte: new Date(now() - WINDOW_MS) }, tenant: "aesthetic",
          piece: { $in: LAKLOK_PIECES }, featureVersion: LAKLOK_FEATURE_VERSION,
          action: { $in: ["piece_opened", LAKLOK_PRESENCE] },
        } },
        { $group: { _id: "$user", at: { $max: "$at" } } },
        { $sort: { at: -1 } },
        { $limit: LIMIT * 2 }, // room for accounts without a handle
      ], { maxTimeMS: 5000 }).toArray();
      const subs = rows.map((row) => row._id).filter((id) => typeof id === "string" && !id.startsWith("sotce-"));
      const handles = await db.collection("@handles")
        .find({ _id: { $in: subs } }, { projection: { handle: 1 } }).toArray();
      const bySub = new Map(handles.map((doc) => [doc._id, doc.handle]));
      const recent = rows
        .filter((row) => bySub.get(row._id))
        .slice(0, LIMIT)
        .map((row) => ({ handle: bySub.get(row._id), at: row.at.toISOString() }));
      return respond(200, { recent, now: new Date(now()).toISOString() },
        { "Cache-Control": "public, max-age=30" });
    } catch (err) {
      console.error("laklok-recent:", err?.message);
      return respond(500, { message: "unavailable" });
    } finally {
      await database?.disconnect?.();
    }
  };
}

export const handler = createLaklokRecentHandler();
