// nom-scores, 2026.08.07
// Public top runs; authenticated writes only for people with an @handle.

import { authorize, getHandleOrEmail } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { compareNomRuns, normalizeNomRun } from "../../public/aesthetic.computer/lib/nom-score.mjs";

const GAMES = new Set([
  "numbnom", "engnom", "mexinom", "dannom", "rusnom", "catnom", "notenom", "artnom",
]);

function gameFrom(event, body = {}) {
  return String(body.game || event.queryStringParameters?.game || "dannom").toLowerCase();
}

function publicRow(row, rank) {
  return {
    rank,
    handle: row.handle,
    colors: Array.isArray(row.colors) ? row.colors : null,
    score: row.score,
    level: row.level,
    correct: row.correct,
    when: row.when,
  };
}

export async function handler(event) {
  let body = {};
  if (event.httpMethod === "POST") {
    try {
      body = JSON.parse(event.body || "{}");
    } catch {
      return respond(400, { message: "Bad JSON." });
    }
  }

  const game = gameFrom(event, body);
  if (!GAMES.has(game)) return respond(400, { message: "Unknown Nom game." });

  const database = await connect();
  const scores = database.db.collection("nom-scores");
  await scores.createIndex({ game: 1, user: 1 }, { unique: true });
  await scores.createIndex({ game: 1, score: -1, level: -1, correct: -1, when: 1 });

  try {
    if (event.httpMethod === "GET") {
      const rows = await scores
        .find({ game })
        .sort({ score: -1, level: -1, correct: -1, when: 1 })
        .limit(10)
        .toArray();
      return respond(200, { game, scores: rows.map((row, index) => publicRow(row, index + 1)) });
    }

    if (event.httpMethod !== "POST")
      return respond(405, { message: "Method Not Allowed" });

    const run = normalizeNomRun(body);
    if (!run) return respond(400, { message: "Need valid { score, level, correct }." });

    let user;
    try {
      user = await authorize(event.headers);
    } catch {
      return respond(401, { message: "Log in to rank a run." });
    }
    if (!user?.sub) return respond(401, { message: "Log in to rank a run." });

    const handle = await getHandleOrEmail(user.sub);
    if (!handle?.startsWith("@"))
      return respond(403, { message: "Set an @handle to rank a run." });

    const handleDoc = await database.db.collection("@handles").findOne({
      handle: { $regex: new RegExp(`^${handle.slice(1).replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}$`, "i") },
    });
    const next = {
      game,
      user: user.sub,
      handle,
      colors: Array.isArray(handleDoc?.colors) ? handleDoc.colors : null,
      ...run,
      when: new Date(),
    };
    const prior = await scores.findOne({ game, user: user.sub });
    const improved = compareNomRuns(next, prior) > 0;
    if (improved)
      await scores.updateOne({ game, user: user.sub }, { $set: next }, { upsert: true });

    const ranked = await scores
      .find({ game })
      .sort({ score: -1, level: -1, correct: -1, when: 1 })
      .limit(1000)
      .toArray();
    const rank = ranked.findIndex((row) => row.user === user.sub) + 1;
    const saved = improved ? next : prior;
    return respond(200, { ok: true, improved, rank: rank || null, run: publicRow(saved, rank || null) });
  } finally {
    await database.disconnect();
  }
}
