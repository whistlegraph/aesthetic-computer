// oskiewar-stats, 26.08.10
// One JSON for the oskiewar wall: how much was played, by how many people, and
// how the reels did.
//
// Nothing here is new measurement. Rounds already land in `oskiewar-replays`
// with everything this needs on them, pops already land in `oskiewar-pops`, and
// the reel ledger already records what was published. This endpoint only asks
// those three the questions a dashboard asks, in one round trip, so the wall
// polls once a minute instead of three times.
//
// A day is a *local* day, because a wall in a room is read by someone standing
// in that room. The boundary is drawn by Mongo with `$dateToString` and a
// timezone rather than by subtracting hours in JS, so the day that spans a
// daylight-saving change is still one day long.

import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { chooseWindow, localDay, MAX_HOURS, WALL_TIMEZONE }
  from "./oskiewar-pops.mjs";

// The pop counter owns the day boundary because it is the thing that has to
// bucket by day permanently; everything here agrees with it by importing it.
export { localDay };

const REPLAYS = "oskiewar-replays";
const POPS = "oskiewar-pops";
const POP_DAYS = "oskiewar-pop-days";
// Reel figures reach here twice over. The ledger file is the record a human
// reads, but it only changes on the machine that publishes and only arrives on
// lith with a deploy — so a nightly insights pull would sit in git while the wall
// kept quoting whatever the last deploy happened to carry. The clockwork posts
// them here as well, and this collection wins when it has anything to say.
const REEL_INSIGHTS = "oskiewar-reel-insights";
const HOUR = 60 * 60 * 1000;
const DAY = 24 * HOUR;
// The wall shows a month of history and a day of detail.
const DAILY_DAYS = 31;
const HOURLY_HOURS = 24;
// A match is won at five rounds — the same threshold `oskiewarMatchCompleted`
// applies to a single demo, expressed here as an aggregation so the count can
// be taken without pulling every document.
const MATCH_WINS = 5;
// The ledger lives beside the reel tooling, not in the database. Resolved from
// this file rather than from cwd so it is found whether lith runs the function
// from `system/` or a bundler moves it.
const LEDGER = join(dirname(fileURLToPath(import.meta.url)),
  "../../../xbox/live/marketing/ledger.json");

// `fighters` holds two names and the dummy is spelled DUMMY, but a name is
// allowed to arrive in any case, so both of these fold case before comparing.
const FIGHTERS_UPPER = { $map: { input: "$fighters", in: { $toUpper: "$$this" } } };
const IS_DUMMY = { $in: ["DUMMY", FIGHTERS_UPPER] };
const IS_COMPLETED = { $gte: [{ $max: "$finalRoundWins" }, MATCH_WINS] };
const count = (condition) => ({ $sum: { $cond: [condition, 1, 0] } });

const dayKey = (field, timezone) =>
  ({ $dateToString: { format: "%Y-%m-%d", date: field, timezone } });

// A round is one document; a match is a series of them. Both are worth showing:
// rounds is the honest measure of play, matches of completion.
export function shapeDaily(rows, days, today) {
  const byDay = new Map(rows.map((row) => [row._id, row]));
  const out = [];
  for (const day of days) {
    const row = byDay.get(day);
    out.push({
      day,
      rounds: row?.rounds || 0,
      matches: row?.matches || 0,
      players: row?.players || 0,
      dummy: row?.dummy || 0,
      today: day === today,
    });
  }
  return out;
}

// The last `HOURLY_HOURS` whole hours, oldest first, so the sparkline reads
// left to right the way time does. Mongo groups by hours-ago; this turns that
// into a dense series with the gaps filled by the zeroes that really happened.
export function shapeHourly(rows, hours = HOURLY_HOURS) {
  const byAge = new Map(rows.map((row) => [Number(row._id), row.rounds]));
  const out = [];
  for (let age = hours - 1; age >= 0; age--) out.push(byAge.get(age) || 0);
  return out;
}

// Stored rows and ledger rows are merged on id, with the stored insights winning
// because they are the ones that can be newer than the deploy. A reel the ledger
// has never heard of still counts — it was published, whatever this checkout
// knows — so the union is taken rather than the ledger's list alone.
export function mergeReelSources(ledger, stored = []) {
  const posts = Array.isArray(ledger?.posts) ? ledger.posts : [];
  const byId = new Map();
  for (const post of posts) {
    if (post.mode === "live") byId.set(post.id, { ...post });
  }
  for (const row of stored) {
    const existing = byId.get(row._id) || {
      mode: "live", id: row._id, day: row.day, segment: row.segment,
      publishedAt: row.publishedAt, urls: { cover: row.cover, reel: row.reel },
    };
    byId.set(row._id, { ...existing, insights: row.insights, insightsAt: row.insightsAt });
  }
  return { posts: [...byId.values()] };
}

// Sum the reels that actually went out. `insights` is null until something pulls
// it, so a reel that has never been measured contributes its existence and
// nothing else — the wall says "2 live, 0 measured" rather than quietly reporting
// zero views as though that were a result.
export function shapeReels(ledger) {
  const posts = Array.isArray(ledger?.posts) ? ledger.posts : [];
  const live = posts.filter((post) => post.mode === "live");
  const measured = live.filter((post) => post.insights);
  const totals = { views: 0, reach: 0, interactions: 0, likes: 0, comments: 0 };
  for (const post of measured) {
    totals.views += post.insights.views || 0;
    totals.reach += post.insights.reach || 0;
    totals.interactions += post.insights.total_interactions || 0;
    totals.likes += post.insights.likes || 0;
    totals.comments += post.insights.comments || 0;
  }
  const watchTimes = measured
    .map((post) => post.insights.ig_reels_avg_watch_time)
    .filter((value) => Number.isFinite(value));
  return {
    live: live.length,
    measured: measured.length,
    totals,
    // Milliseconds, as the Graph API reports it.
    avgWatchTime: watchTimes.length
      ? watchTimes.reduce((sum, value) => sum + value, 0) / watchTimes.length
      : null,
    posts: live
      .slice()
      .sort((a, b) => String(b.publishedAt).localeCompare(String(a.publishedAt)))
      .map((post) => ({
        id: post.id,
        day: post.day,
        segment: post.segment,
        publishedAt: post.publishedAt,
        cover: post.urls?.cover || null,
        // The reel itself, so a wall can loop the thing it is reporting on. It is
        // already a public object on Spaces — the same URL Meta was handed to
        // fetch the video at publish time.
        reel: post.urls?.reel || null,
        insights: post.insights || null,
      })),
  };
}

// Play numbers are public — the pop counter on the title screen already shares
// them, and a wall in a room is not a secret. Reel performance is different: it
// is marketing data about an account, so it travels only to a caller holding the
// wall key. With no key configured nothing is disclosed, because the failure a
// public endpoint must not have is the one where it quietly answers everybody.
function reelsPermitted(event) {
  const expected = process.env.OSKIEWAR_WALL_KEY;
  if (!expected) return false;
  const offered = event.queryStringParameters?.key ||
    event.headers?.["x-oskiewar-wall-key"];
  return typeof offered === "string" && offered === expected;
}

function readLedger() {
  // A missing or half-written ledger must not take the wall down with it — the
  // play numbers are the point and they come from somewhere else entirely.
  try {
    return JSON.parse(readFileSync(LEDGER, "utf8"));
  } catch {
    return null;
  }
}

// The clockwork's half of the loop: whatever it pulled from Meta is upserted per
// reel, keyed by the ledger's own id. Only the wall key opens this, since it is
// the same secret that is allowed to read the figures back out.
async function storeReelInsights(database, posts) {
  const rows = (Array.isArray(posts) ? posts : [])
    .filter((post) => post && typeof post.id === "string" && post.insights);
  if (!rows.length) return 0;
  const collection = database.db.collection(REEL_INSIGHTS);
  await collection.bulkWrite(rows.map((post) => ({
    updateOne: {
      filter: { _id: post.id },
      update: {
        $set: {
          day: post.day ?? null,
          segment: post.segment ?? null,
          publishedAt: post.publishedAt ?? null,
          cover: post.urls?.cover ?? post.cover ?? null,
          reel: post.urls?.reel ?? post.reel ?? null,
          insights: post.insights,
          insightsAt: post.insightsAt || new Date().toISOString(),
        },
      },
      upsert: true,
    },
  })), { ordered: false });
  return rows.length;
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");

  if (event.httpMethod === "POST") {
    if (!reelsPermitted(event))
      return respond(403, { error: "Wall key required" });
    let database;
    try {
      const body = JSON.parse(event.body || "{}");
      database = await connect();
      const stored = await storeReelInsights(database, body.posts);
      await database.disconnect();
      return respond(200, { ok: true, stored });
    } catch (error) {
      if (database) await database.disconnect();
      return respond(500, { error: error.message || "Insight storage failed" });
    }
  }

  if (event.httpMethod !== "GET")
    return respond(405, { error: "Method not allowed" });

  const timezone = event.queryStringParameters?.tz || WALL_TIMEZONE;
  const wantsReels = reelsPermitted(event);
  let database;
  try {
    database = await connect();
    const now = new Date();
    const today = localDay(now, timezone);
    const key = dayKey("$recordedAt", timezone);
    const replays = database.db.collection(REPLAYS);

    const [facets, popRows, popDays, storedReels] = await Promise.all([
      replays.aggregate([{
        $facet: {
          // Everything ever recorded, in one pass.
          totals: [{
            $group: {
              _id: null,
              rounds: { $sum: 1 },
              matches: count(IS_COMPLETED),
              dummy: count(IS_DUMMY),
              latestAt: { $max: "$recordedAt" },
              series: { $addToSet: "$seriesId" },
            },
          }],
          // A player is a fighter who is not the dummy. Counting distinct names
          // is what answers "how many people have played" — the same person on
          // two nights is one player, which is the honest reading.
          players: [
            { $unwind: "$fighters" },
            { $match: { $expr: { $ne: [{ $toUpper: "$fighters" }, "DUMMY"] } } },
            { $group: { _id: { $toUpper: "$fighters" } } },
            { $count: "distinct" },
          ],
          playersToday: [
            { $match: { $expr: { $eq: [key, today] } } },
            { $unwind: "$fighters" },
            { $match: { $expr: { $ne: [{ $toUpper: "$fighters" }, "DUMMY"] } } },
            { $group: { _id: { $toUpper: "$fighters" } } },
            { $count: "distinct" },
          ],
          daily: [
            { $match: { recordedAt: { $gte: new Date(now - DAILY_DAYS * DAY) } } },
            {
              $group: {
                _id: key,
                rounds: { $sum: 1 },
                matches: count(IS_COMPLETED),
                dummy: count(IS_DUMMY),
                fighters: { $addToSet: FIGHTERS_UPPER },
              },
            },
            {
              $project: {
                rounds: 1, matches: 1, dummy: 1,
                players: {
                  $size: {
                    $filter: {
                      input: { $setDifference: [
                        { $reduce: { input: "$fighters", initialValue: [],
                          in: { $setUnion: ["$$value", "$$this"] } } },
                        ["DUMMY"],
                      ] },
                      cond: { $ne: ["$$this", null] },
                    },
                  },
                },
              },
            },
          ],
          hourly: [
            { $match: { recordedAt: { $gte: new Date(now - HOURLY_HOURS * HOUR) } } },
            {
              $group: {
                _id: { $floor: { $divide: [
                  { $subtract: [now, "$recordedAt"] }, HOUR] } },
                rounds: { $sum: 1 },
              },
            },
          ],
        },
      }]).toArray(),
      // Pops carry only the hour they happened, so the same hour-bucket
      // aggregation the pop counter runs is the only shape available here.
      database.db.collection(POPS).aggregate([
        { $match: { at: { $gte: new Date(now - MAX_HOURS * HOUR) } } },
        {
          $group: {
            _id: { $floor: { $divide: [{ $subtract: [now, "$at"] }, HOUR] } },
            pops: { $sum: 1 },
          },
        },
      ]).toArray(),
      // The day counters the pop endpoint keeps outside the TTL sweep. They only
      // begin where that rollup was deployed, so `since` travels with the total
      // and the wall labels it rather than passing it off as all-time.
      database.db.collection(POP_DAYS).find({})
        .sort({ _id: 1 }).limit(400).toArray(),
      // Only worth fetching for a caller who may read them back out.
      wantsReels
        ? database.db.collection(REEL_INSIGHTS).find({}).limit(400).toArray()
        : Promise.resolve([]),
    ]);

    await database.disconnect();

    const facet = facets[0] || {};
    const totals = facet.totals?.[0] || {};
    const days = [];
    for (let back = DAILY_DAYS - 1; back >= 0; back--) {
      days.push(localDay(new Date(now.getTime() - back * DAY), timezone));
    }
    const daily = shapeDaily(facet.daily || [], days, today);
    const todayRow = daily.find((row) => row.today) ||
      { rounds: 0, matches: 0, players: 0, dummy: 0 };

    const byHour = [];
    for (const row of popRows) byHour[Number(row._id)] = row.pops;
    const window = chooseWindow(byHour);

    // The rollup is authoritative for a whole day; the raw rows are the only
    // thing that can speak for a day still in progress. So today comes from the
    // rows and every finished day comes from the counter.
    const popsByDay = new Map(popDays.map((row) => [row._id, row.pops || 0]));
    const popsToday = byHour
      .slice(0, HOURLY_HOURS)
      .reduce((sum, value) => sum + (value || 0), 0);
    for (const row of daily) {
      row.pops = row.today ? popsToday : (popsByDay.get(row.day) || 0);
    }
    const popsCounted = popDays.reduce((sum, row) => sum + (row.pops || 0), 0);

    const seriesEver = (totals.series || []).filter(Boolean).length;
    const rounds = totals.rounds || 0;

    return respond(200, {
      format: "ac.oskiewar.stats",
      version: 1,
      generatedAt: now.toISOString(),
      timezone,
      day: today,
      today: {
        rounds: todayRow.rounds,
        matches: todayRow.matches,
        players: todayRow.players,
        // Split of today's rounds by who was on the other side.
        dummy: todayRow.dummy,
        localPlayer: todayRow.rounds - todayRow.dummy,
        pops: popsToday,
      },
      allTime: {
        rounds,
        matches: totals.matches || 0,
        series: seriesEver,
        players: facet.players?.[0]?.distinct || 0,
        dummy: totals.dummy || 0,
        localPlayer: rounds - (totals.dummy || 0),
        // Of the rounds recorded, how many ended a match.
        completionRatio: rounds ? (totals.matches || 0) / rounds : 0,
        latestAt: totals.latestAt || null,
      },
      // `window` is what the pop counter itself would quote, kept here so the
      // wall cannot contradict the site. `counted` is the swept-proof total, and
      // it carries the day it starts from because it is not all of history —
      // only history since the rollup began.
      pops: {
        ...window,
        maxHours: MAX_HOURS,
        counted: popsCounted,
        since: popDays[0]?._id || null,
        days: popDays.length,
      },
      hourly: shapeHourly(facet.hourly || []),
      daily,
      reels: wantsReels
        ? shapeReels(mergeReelSources(readLedger(), storedReels))
        : null,
    }, {
      // The wall polls this; a minute stale costs nothing and keeps a reload
      // storm off the database.
      "Cache-Control": "public, max-age=60",
    });
  } catch (error) {
    if (database) await database.disconnect();
    return respond(500, { error: error.message || "Stats failed" });
  }
}
