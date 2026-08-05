// oskiewar-replays, 26.08.04
// Stores and serves compact, versioned AC demo streams.

import { createHmac } from "node:crypto";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import {
  oskiewarEvent,
  oskiewarMatchCompleted,
  oskiewarReplayProperties,
} from "../../public/aesthetic.computer/lib/oskiewar-analytics.mjs";
import { createPostHogEventCapture } from "../../../shared/posthog-event-capture.mjs";

const COLLECTION = "oskiewar-replays";
const MAX_BYTES = 524288;
const MATCH_WORD = "[bdfgklmnprstvz][aeiou][bdfgklmnprstvz][aeiou][bdfgklmnprstvz][aeiou]";
const MATCH_NAME = new RegExp(
  `^(?:${MATCH_WORD}-${MATCH_WORD}-${MATCH_WORD}|[a-z]{4,7}[0-9]{1,3})$`);
const MATCH_ID = new RegExp(
  `^ow-(?:${MATCH_WORD}-${MATCH_WORD}-${MATCH_WORD}|[a-z]{4,7}[0-9]{1,3})$`);
const oskiewarAnalytics = createPostHogEventCapture({
  distinctId: "ac-oskiewar-lith-aggregate",
  eventFactory: oskiewarEvent,
});

const finite = (value, limit = 1000000000) =>
  Number.isFinite(value) && Math.abs(value) <= limit;
const numericRow = (row, length) => Array.isArray(row) && row.length === length &&
  row.every((value) => finite(value));

export function validateDemo(value) {
  if (!value || value.format !== "ac.oskiedemo" || value.version !== 1 ||
      value.game !== "oskiewar" || value.simulation !== "oskiewar-physics-1" ||
      value.tickRate !== 60) return "Unsupported demo format";
  const namedMatch = typeof value.matchName === "string";
  if (namedMatch && (!MATCH_NAME.test(value.matchName) ||
      value.matchId !== "ow-" + value.matchName)) return "Invalid match name";
  if (!namedMatch && !/^ow-[a-z0-9]+-[a-z0-9]+$/.test(value.matchId || ""))
    return "Invalid matchId";
  const linkedRound = value.roundId !== undefined || value.seriesId !== undefined;
  if (linkedRound && (!MATCH_ID.test(value.roundId || "") ||
      value.roundId !== value.matchId || value.roundName !== value.matchName ||
      !MATCH_ID.test(value.seriesId || "") ||
      value.seriesId !== "ow-" + value.seriesName ||
      !Array.isArray(value.roundIds) || value.roundIds.length < 1 ||
      value.roundIds.length > 32 || value.roundIds.some((id) => !MATCH_ID.test(id)) ||
      !Number.isInteger(value.roundIndex) || value.roundIndex < 0 ||
      value.roundIndex >= value.roundIds.length ||
      value.roundIds[value.roundIndex] !== value.roundId ||
      (value.previousRoundId !== "" && !MATCH_ID.test(value.previousRoundId || "")) ||
      (value.roundIndex === 0 && value.previousRoundId !== "") ||
      (value.roundIndex > 0 &&
        value.previousRoundId !== value.roundIds[value.roundIndex - 1])))
    return "Invalid round linkage";
  if (!finite(value.startedAt, 10000000000000) ||
      !Number.isInteger(value.durationTicks) || value.durationTicks < 0 ||
      value.durationTicks > 216000) return "Invalid timing";
  if (!Array.isArray(value.fighters) || value.fighters.length !== 2 ||
      value.fighters.some((fighter) => typeof fighter !== "string" ||
        !/^@?[A-Z0-9_-]{1,24}$/i.test(fighter))) return "Invalid fighters";
  if (value.winner !== null && value.winner !== undefined &&
      !value.fighters.includes(value.winner)) return "Invalid winner";
  if (!Array.isArray(value.finalRoundWins) || value.finalRoundWins.length !== 2 ||
      value.finalRoundWins.some((score) => !Number.isInteger(score) ||
        score < 0 || score > 5)) return "Invalid result";
  if (!Array.isArray(value.commands) || value.commands.length > 50000 ||
      value.commands.some((row) => !numericRow(row, 3) ||
        !Number.isInteger(row[0]) || ![0, 1].includes(row[1]) ||
        !Number.isInteger(row[2]) || row[2] < 0 || row[2] > 255))
    return "Invalid commands";
  if (!Array.isArray(value.events) || value.events.length > 10000 ||
      value.events.some((row) => !Array.isArray(row) || row.length !== 5 ||
        !finite(row[0]) || typeof row[1] !== "string" ||
        !/^[a-z0-9_-]{1,32}$/.test(row[1]) ||
        !finite(row[2]) || !finite(row[3]) || !finite(row[4])))
    return "Invalid events";
  if (!Array.isArray(value.checkpoints) || value.checkpoints.length > 4000 ||
      value.checkpoints.some((row) => !numericRow(row, 26)))
    return "Invalid checkpoints";
  if (!Array.isArray(value.rounds) || value.rounds.length > 128 ||
      value.rounds.some((row) => !numericRow(row, 4))) return "Invalid rounds";
  return null;
}

function sourceDigest(event) {
  const headers = event.headers || {};
  const ip = headers["cf-connecting-ip"] ||
    headers["x-forwarded-for"]?.split(",")[0]?.trim() || "unknown";
  const secret = process.env.REPLAY_HASH_SECRET || process.env.JWT_SECRET ||
    "ac-replay-rate-v1";
  return createHmac("sha256", secret).update(ip).digest("hex");
}

function publicReplay(document) {
  if (!document) return null;
  const { sourceDigest: _, ...safe } = document;
  return { ...safe, id: document._id, _id: undefined };
}

export function captureStoredReplay(
  demo,
  surface,
  capture = oskiewarAnalytics.capture,
) {
  const properties = oskiewarReplayProperties(demo, surface);
  capture("round_stored", properties);
  if (oskiewarMatchCompleted(demo)) {
    capture("match_completed", properties);
  }
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");
  let database;
  try {
    database = await connect();
    const collection = database.db.collection(COLLECTION);

    if (event.httpMethod === "GET") {
      const params = event.queryStringParameters || {};
      if (params.id) {
        const replay = await collection.findOne({ _id: params.id });
        await database.disconnect();
        return replay ? respond(200, { replay: publicReplay(replay) })
          : respond(404, { error: "Replay not found" });
      }
      if (params.series) {
        if (!MATCH_ID.test(params.series)) {
          await database.disconnect();
          return respond(400, { error: "Invalid series ID" });
        }
        const rows = await collection.find({ seriesId: params.series }, {
          projection: { commands: 0, events: 0, checkpoints: 0, rounds: 0,
            sourceDigest: 0 },
        }).sort({ roundIndex: 1 }).limit(32).toArray();
        await database.disconnect();
        return respond(200, { seriesId: params.series,
          rounds: rows.map(publicReplay) });
      }
      const limit = Math.min(50, Math.max(1, Number(params.limit || 20)));
      const [matchesPlayed, latest, rows] = await Promise.all([
        collection.countDocuments({}),
        collection.find({}).sort({ recordedAt: -1 }).limit(1).next(),
        collection.find({}, { projection: { commands: 0, events: 0,
          checkpoints: 0, rounds: 0, sourceDigest: 0 } })
          .sort({ recordedAt: -1 }).limit(limit).toArray(),
      ]);
      await database.disconnect();
      return respond(200, {
        format: "ac.oskiedemo", version: 1, matchesPlayed,
        latestAt: latest?.recordedAt || null,
        replays: rows.map(publicReplay),
      });
    }

    if (event.httpMethod !== "POST") {
      await database.disconnect();
      return respond(405, { error: "Method not allowed" });
    }
    if (Buffer.byteLength(event.body || "", "utf8") > MAX_BYTES) {
      await database.disconnect();
      return respond(413, { error: "Demo exceeds 512 KiB" });
    }
    let demo;
    try { demo = JSON.parse(event.body || "{}"); }
    catch {
      await database.disconnect();
      return respond(400, { error: "Invalid JSON" });
    }
    const invalid = validateDemo(demo);
    if (invalid) {
      await database.disconnect();
      return respond(400, { error: invalid });
    }
    const digest = sourceDigest(event);
    const recent = await collection.countDocuments({ sourceDigest: digest,
      recordedAt: { $gte: new Date(Date.now() - 60 * 60 * 1000) } });
    if (recent >= 120) {
      await database.disconnect();
      return respond(429, { error: "Replay rate limit reached" });
    }
    const recordedAt = new Date();
    const result = await collection.updateOne({ _id: demo.matchId }, {
      $setOnInsert: { _id: demo.matchId, ...demo, recordedAt,
        sourceDigest: digest },
    }, { upsert: true });
    await database.disconnect();
    if (result.upsertedCount) {
      captureStoredReplay(demo, event.queryStringParameters?.surface);
    }
    return respond(result.upsertedCount ? 201 : 200, {
      ok: true, id: demo.matchId, stored: Boolean(result.upsertedCount),
      replay: `/api/oskiewar-replays?id=${encodeURIComponent(demo.matchId)}`,
    });
  } catch (error) {
    if (database) await database.disconnect();
    return respond(500, { error: error.message || "Replay storage failed" });
  }
}
