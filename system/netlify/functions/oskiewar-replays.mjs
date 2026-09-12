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
import {
  countryFromHeaders,
  normalizeCountry,
  trustedFighterNations,
} from "../../backend/oskiewar-country.mjs";

const COLLECTION = "oskiewar-replays";
const MAX_BYTES = 524288;
const MATCH_WORD = "[bdfgklmnprstvz][aeiou][bdfgklmnprstvz][aeiou][bdfgklmnprstvz][aeiou]";
const MATCH_NAME = new RegExp(
  `^(?:${MATCH_WORD}-${MATCH_WORD}-${MATCH_WORD}|[a-z]{4,7}[0-9]{1,3})$`);
const MATCH_ID = new RegExp(
  `^ow-(?:${MATCH_WORD}-${MATCH_WORD}-${MATCH_WORD}|[a-z]{4,7}[0-9]{1,3})$`);
// A round summary: everything the ledgers fold, and none of the bulk. A room
// with five hundred rounds in it must not drag five hundred command streams
// across the wire to answer "who has played here".
const ROUND_SUMMARY = { seriesId: 1, seriesName: 1, roundIndex: 1, roomId: 1,
  roomName: 1, fighters: 1, winner: 1, finalRoundWins: 1, recordedAt: 1 };
const ROOM_ROUND_LIMIT = 500;
const FIGHTER_ROUND_LIMIT = 1000;
const FIGHTER_NAME = /^@?[A-Z0-9_-]{1,24}$/;
// The same case-folded read of a row's two names that oskiewar-stats uses.
const FIGHTERS_UPPER =
  { $map: { input: "$fighters", in: { $toUpper: "$$this" } } };

export function canonicalRoomId(value) {
  const raw = String(value || "").toLowerCase();
  const name = raw.startsWith("ow-") ? raw.slice(3) : raw;
  return MATCH_NAME.test(name) ? `ow-${name}` : null;
}

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
  // Which room this round was played in. Only a versus fight has one: in the
  // broadcast lane each round IS its own room and the round id already says
  // so. It is what makes a room answerable for its own history.
  if (value.roomId !== undefined &&
      (!MATCH_ID.test(value.roomId) || typeof value.roomName !== "string" ||
       value.roomId !== "ow-" + value.roomName)) return "Invalid room";
  // Whether the round ran on a clock. Versus rounds do not, and a re-run
  // handed a countdown the original never had ends early on a fight the
  // original played past. Demos recorded before this field were all timed.
  if (value.timed !== undefined && typeof value.timed !== "boolean")
    return "Invalid timing flag";
  if (!finite(value.startedAt, 10000000000000) ||
      !Number.isInteger(value.durationTicks) || value.durationTicks < 0 ||
      value.durationTicks > 216000) return "Invalid timing";
  if (!Array.isArray(value.fighters) || value.fighters.length !== 2 ||
      value.fighters.some((fighter) => typeof fighter !== "string" ||
        // The signed-out seat wears a blank nameplate on purpose, so the
        // empty string is a legitimate fighter — rejecting it silently
        // discarded every anonymous player's round for seventeen days.
        (fighter !== "" && !/^@?[A-Z0-9_-]{1,24}$/i.test(fighter))))
    return "Invalid fighters";
  // Optional for compatibility with every demo recorded before nations were
  // introduced. The server replaces this field at write time, but validating
  // its shape keeps the version-1 document contract bounded.
  if (value.nations !== undefined &&
      (!Array.isArray(value.nations) || value.nations.length !== 2 ||
       // The piece writes `player.nation || ""` — an unresolved country
       // arrives as the empty string, the same unknown null spells. The
       // server rewrites this field at write time anyway.
       value.nations.some((country) => country !== null && country !== "" &&
         normalizeCountry(country) === null))) return "Invalid nations";
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
  // Two checkpoint generations: 26 columns from tower-era clients, and 32
  // once the full camera-doll pose started riding along (position, fov,
  // perspective, roll) so replays could stand the lens where the live pass
  // stood it. That growth on 2026-08-10 is what silently 400'd every upload
  // for seventeen days — the row got richer and the contract never heard.
  if (!Array.isArray(value.checkpoints) || value.checkpoints.length > 4000 ||
      value.checkpoints.some((row) => !numericRow(row, 26) &&
        !numericRow(row, 32)))
    return "Invalid checkpoints";
  if (!Array.isArray(value.rounds) || value.rounds.length > 128 ||
      value.rounds.some((row) => !numericRow(row, 4))) return "Invalid rounds";
  return null;
}

// A match is won at five rounds — the game's own `matchWins`. Kept in step
// with oskiewar-stats.mjs, which asks the same question of the same rows.
const MATCH_WINS = 5;
// Handles are written uppercase by the piece, but a row is only ever as
// careful as the build that wrote it, so every comparison here folds case.
const upper = (value) => String(value || "").toUpperCase();
// The signed-out seat wears a blank nameplate on purpose. It is a legitimate
// fighter and its rounds are kept, but it is not a person and cannot hold a
// record — every anonymous player would otherwise share one.
const named = (value) => upper(value) !== "";

// Round documents, folded into the matches they belong to. One series is one
// match: up to 32 rounds sharing a `seriesId`, with `finalRoundWins` carrying
// the running tally, so the last round of a series holds its final score.
export function foldMatches(rows = []) {
  const series = new Map();
  for (const row of rows) {
    const id = row?.seriesId || row?._id;
    if (!id) continue;
    if (!series.has(id)) series.set(id, []);
    series.get(id).push(row);
  }
  return [...series.values()].map(foldOneMatch)
    .sort((a, b) => (b.endedAt || "").localeCompare(a.endedAt || ""));
}

// Which seat won a round. The name on `winner` cannot answer this on its own:
// a signed-out fighter's nameplate is the empty string on purpose, so a round
// an anonymous player WON is stored indistinguishably from a tie — and every
// loss to an anonymous player would quietly vanish from the other player's
// record. The running tally does answer it: the seat whose `finalRoundWins`
// went up is the seat that won, and a round where neither moved was a tie.
// The name is the fallback for the first round of a window that does not
// start at the beginning of its match.
function roundWinnerSeat(row, before) {
  if (before) {
    const now = row.finalRoundWins || [0, 0];
    for (let seat = 0; seat < 2; seat++)
      if ((now[seat] || 0) > (before[seat] || 0)) return seat;
    return -1;
  }
  if (!row.winner) return -1;
  const seat = (row.fighters || [])
    .findIndex((name, position) => position < 2 && upper(name) === upper(row.winner));
  return seat;
}

function foldOneMatch(rows) {
  const ordered = rows.slice().sort((a, b) =>
    (a.roundIndex ?? 0) - (b.roundIndex ?? 0));
  const newest = ordered[ordered.length - 1];
  const wins = [0, 0];
  let ties = 0;
  let before = ordered[0]?.roundIndex === 0 ? [0, 0] : null;
  for (const row of ordered) {
    const seat = roundWinnerSeat(row, before);
    if (seat >= 0) wins[seat]++; else ties++;
    before = (row.finalRoundWins || [0, 0]).slice(0, 2);
  }
  const times = rows.map((row) => row.recordedAt
    ? new Date(row.recordedAt).getTime() : 0).filter(Boolean);
  const score = (newest?.finalRoundWins || [0, 0]).slice(0, 2);
  const top = Math.max(...score, 0);
  const seat = score.indexOf(top);
  const decided = top >= MATCH_WINS && score[1 - seat] < top;
  const fighters = (newest?.fighters || []).slice(0, 2);
  const stamp = (value) =>
    Number.isFinite(value) && value > 0 ? new Date(value).toISOString() : null;
  return {
    seriesId: newest?.seriesId || newest?._id || "",
    seriesName: newest?.seriesName || "",
    roomId: newest?.roomId || "", roomName: newest?.roomName || "",
    fighters, score, rounds: ordered.length, roundWins: wins, ties,
    // An anonymous winner is a real winner wearing a blank nameplate, so the
    // name may be "" while `complete` says the match was decided.
    winner: decided ? (fighters[seat] ?? "") : null, complete: decided,
    roundIds: ordered.map((row) => row._id),
    startedAt: stamp(times.length ? Math.min(...times) : 0),
    endedAt: stamp(times.length ? Math.max(...times) : 0),
  };
}

// Every named fighter in a set of matches, and how they did. This is the
// track record: rounds are the honest unit because every round document has a
// winner, and matches are the ones that reached five.
export function standings(matches = []) {
  const players = new Map();
  const seat = (handle) => {
    const key = upper(handle);
    if (!players.has(key)) players.set(key, { handle: key,
      roundsWon: 0, roundsLost: 0, ties: 0,
      matchesPlayed: 0, matchesWon: 0, opponents: [], lastAt: null });
    return players.get(key);
  };
  for (const match of matches) {
    const pair = match.fighters || [];
    for (let index = 0; index < 2; index++) {
      if (!named(pair[index])) continue;
      const player = seat(pair[index]);
      player.roundsWon += match.roundWins[index] || 0;
      player.roundsLost += match.roundWins[1 - index] || 0;
      player.ties += match.ties || 0;
      player.matchesPlayed++;
      if (match.complete && upper(match.winner) === upper(pair[index]))
        player.matchesWon++;
      if (named(pair[1 - index]) &&
          !player.opponents.includes(upper(pair[1 - index])))
        player.opponents.push(upper(pair[1 - index]));
      if (!player.lastAt || (match.endedAt || "") > player.lastAt)
        player.lastAt = match.endedAt;
    }
  }
  return [...players.values()]
    .sort((a, b) => b.matchesWon - a.matchesWon ||
      b.roundsWon - a.roundsWon || a.handle.localeCompare(b.handle));
}

// One room's ledger: the matches played at its address, newest first, and
// what everybody who played there has to show for it.
export function roomHistory(roomId, rows = []) {
  const matches = foldMatches(rows);
  return { roomId, room: String(roomId || "").replace(/^ow-/, ""),
    matches, rounds: rows.length, players: standings(matches) };
}

// One handle's track record, across every room they have played in.
export function trackRecord(handle, rows = []) {
  const key = upper(handle);
  const matches = foldMatches(rows)
    .filter((match) => (match.fighters || []).some((name) => upper(name) === key));
  const record = standings(matches).find((player) => player.handle === key) ||
    { handle: key, roundsWon: 0, roundsLost: 0, ties: 0,
      matchesPlayed: 0, matchesWon: 0, opponents: [], lastAt: null };
  const rooms = [...new Set(matches.map((match) => match.roomId).filter(Boolean))];
  return { ...record, rooms, matches };
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

export function normalizeNationRows(rows = [], matchesPlayed = 0) {
  const nations = rows
    .map((row) => ({ country: normalizeCountry(row?._id),
      games: Number(row?.games) || 0 }))
    .filter((row) => row.country && row.games > 0)
    .sort((a, b) => b.games - a.games || a.country.localeCompare(b.country));
  const knownGames = nations.reduce((total, row) => total + row.games, 0);
  return { matchesPlayed, knownGames,
    unknownGames: Math.max(0, matchesPlayed - knownGames), nations };
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
      if (params.stats === "nations") {
        const [matchesPlayed, rows] = await Promise.all([
          collection.countDocuments({}),
          collection.aggregate([
            { $match: { country: { $type: "string" } } },
            { $group: { _id: "$country", games: { $sum: 1 } } },
            { $sort: { games: -1, _id: 1 } },
          ]).toArray(),
        ]);
        await database.disconnect();
        return respond(200, normalizeNationRows(rows, matchesPlayed), {
          "Cache-Control": "public, max-age=60",
        });
      }
      if (params.id) {
        const replay = await collection.findOne({ _id: params.id });
        await database.disconnect();
        return replay ? respond(200, { replay: publicReplay(replay) })
          : respond(404, { error: "Replay not found" });
      }
      // A room's own ledger. A versus room is a lasting address — the link a
      // friend was sent — so it accumulates every match ever played at it,
      // which is the thing `?series=` below cannot answer: a series is one
      // match, a room is all of them.
      if (params.room) {
        const roomId = canonicalRoomId(params.room);
        if (!roomId) {
          await database.disconnect();
          return respond(400, { error: "Invalid room ID" });
        }
        // Declared on the read path because it is idempotent and this
        // endpoint has no deploy step of its own to hang it off.
        await collection.createIndex({ roomId: 1, recordedAt: -1 })
          .catch(() => {});
        const rows = await collection.find({ roomId }, {
          projection: ROUND_SUMMARY,
        }).sort({ recordedAt: -1 }).limit(ROOM_ROUND_LIMIT).toArray();
        await database.disconnect();
        return respond(200, roomHistory(roomId, rows), {
          "Cache-Control": "public, max-age=30",
        });
      }
      // One player's track record, across every room they have played in.
      if (params.fighter) {
        const handle = String(params.fighter).toUpperCase();
        if (!FIGHTER_NAME.test(handle)) {
          await database.disconnect();
          return respond(400, { error: "Invalid fighter" });
        }
        // Names are matched with case folded, the same way oskiewar-stats
        // asks the question, so a row written by an older build still counts.
        const rows = await collection.find({
          $expr: { $in: [handle, FIGHTERS_UPPER] },
        }, { projection: ROUND_SUMMARY })
          .sort({ recordedAt: -1 }).limit(FIGHTER_ROUND_LIMIT).toArray();
        await database.disconnect();
        return respond(200, trackRecord(handle, rows), {
          "Cache-Control": "public, max-age=30",
        });
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
    const country = countryFromHeaders(event.headers);
    const nations = trustedFighterNations(demo.fighters, country);
    // Country is server-owned. Drop any body value so a client cannot choose
    // its flag or poison the aggregate.
    const {
      country: _untrustedCountry,
      nations: _untrustedNations,
      ...safeDemo
    } = demo;
    const result = await collection.updateOne({ _id: demo.matchId }, {
      $setOnInsert: { _id: demo.matchId, ...safeDemo, recordedAt,
        nations, ...(country ? { country } : {}), sourceDigest: digest },
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
