// Saved Oskiewar rounds are public MIME works, addressed by their existing ID.
export const REPLAY_COLLECTION = "oskiewar-replays";
export const REPLAY_TYPE = "application/x-oskiewar-demo";
export const REPLAY_ID = /^ow-[a-z0-9]+(?:-[a-z0-9]+){0,3}$/;
const NAMED_ROUND = /^(?:[a-z]{4,7}[0-9]{1,3}|(?:[bdfgklmnprstvz][aeiou]){3}(?:-(?:[bdfgklmnprstvz][aeiou]){3}){2})$/;
export const REPLAY_VISIBLE = { format: "ac.oskiedemo", game: "oskiewar",
  deleted: { $ne: true }, hidden: { $ne: true }, private: { $ne: true },
  visibility: { $nin: ["private", "unlisted"] } };
// Never move command streams, source digests or machine metadata into the feed.
export const REPLAY_FIELDS = { _id: 1, matchId: 1, matchName: 1, recordedAt: 1,
  fighters: 1, winner: 1, durationTicks: 1, tickRate: 1,
  version: 1, simulation: 1, map: 1, build: 1 };

const CANDIDATES = 1000;
const CONTACTS = ["bodyhit", "part-hit", "block", "ballblock", "shield-bash",
  "grab-player", "throw-player", "wack", "boot", "balled", "ko"];
const ACTIONS = [...CONTACTS, "punch", "kick", "whip", "bash", "spit", "bullet",
  "laser", "rocket", "grenade", "saber", "throw", "throw-part", "board-kick"];
const array = value => ({ $cond: [{ $isArray: value }, value, []] });
const at = (value, index) => ({ $arrayElemAt: [value, index] });
// Return only unique tick numbers, not the command/event payloads. Duplicate
// signals on one frame and blood/countdown/ambient effects cannot inflate it.
function ticks(field, conditions) {
  return { $setUnion: [{ $map: { input: { $filter: {
    input: { $filter: { input: array(field), as: "row", cond: { $isArray: "$$row" } } },
    as: "row", cond: { $and: [
      { $isNumber: at("$$row", 0) },
      { $gte: [at("$$row", 0), "$_roundStart"] },
      { $lte: [at("$$row", 0), "$durationTicks"] }, ...conditions,
    ] },
  } }, as: "row", in: at("$$row", 0) } }, []] };
}

export function replayCandidatePipeline() {
  return [
    { $match: { ...REPLAY_VISIBLE, version: 1, simulation: "oskiewar-physics-1", tickRate: 60 } },
    { $sort: { recordedAt: -1, _id: -1 } },
    { $limit: CANDIDATES },
    // Saved rounds contain the series so far. Match the viewer's roundIndex
    // (or last-round fallback); an earlier busy round cannot qualify this one.
    { $set: { _roundStart: { $let: { vars: {
      round: at(array("$rounds"), { $convert: { input: "$roundIndex", to: "int", onNull: -1, onError: -1 } }),
    }, in: { $ifNull: [at(array("$$round"), 0), 0] } } } } },
    { $match: { $expr: { $and: [
      { $isNumber: "$_roundStart" }, { $isNumber: "$durationTicks" },
      { $gte: ["$_roundStart", 0] }, { $gt: ["$durationTicks", "$_roundStart"] },
    ] } } },
    { $project: { ...REPLAY_FIELDS, _activity: {
      start: "$_roundStart", end: "$durationTicks",
      inputs: [0, 1].map(seat => ticks("$commands", [
        { $eq: [at("$$row", 1), seat] }, { $gt: [at("$$row", 2), 0] },
        { $lte: [at("$$row", 2), 255] },
      ])),
      actions: ticks("$events", [{ $in: [at("$$row", 1), ACTIONS] }]),
      contacts: ticks("$events", [{ $in: [at("$$row", 1), CONTACTS] }]),
      frames: ticks("$checkpoints", [{ $in: [{ $size: "$$row" }, [26, 32]] }]),
    } } },
  ];
}

export function replayIsDense(row) {
  const a = row?._activity;
  if (!a || !Number.isFinite(a.start) || !Number.isFinite(a.end)) return false;
  const seconds = (a.end - a.start) / 60;
  const valid = values => [...new Set((Array.isArray(values) ? values : [])
    .filter(t => Number.isFinite(t) && t >= a.start && t <= a.end))].sort((x, y) => x - y);
  const actions = valid(a.actions), contacts = valid(a.contacts), frames = valid(a.frames);
  if (seconds < 6 || ![0, 1].every(seat => valid(a.inputs?.[seat]).length >= 4) ||
      frames.length < 4 || frames.at(-1) - frames[0] < 180 ||
      actions.length < 12 || contacts.length < 3 || actions.length / seconds < .75) return false;
  // A fight must begin promptly and stay busy, including its tail. A burst
  // of attacks followed by an idle timeout still makes a dead feed card.
  if (actions[0] - a.start > 300) return false;
  return [...actions, a.end].every((tick, i) => tick - (i ? actions[i - 1] : a.start) <= 360);
}

export function replayPost(row) {
  const id = row?._id || row?.matchId;
  if (typeof id !== "string" || !REPLAY_ID.test(id) || id.length > 80) return null;
  const fighters = (row.fighters || []).slice(0, 2).map(name =>
    typeof name === "string" && /^@?[a-z0-9_-]{1,24}$/i.test(name) ? name : "");
  const name = fighters.find(name => name.startsWith("@"))?.toLowerCase() || null;
  const named = NAMED_ROUND.test(id.slice(3));
  const url = named ? "https://oskiewar.com/" + encodeURIComponent(id.slice(3)) + "?mime=1"
    : "https://oskiewar.com/mac-test.html?mime=1&replay=" + encodeURIComponent(id);
  return {
    code: id, parent: null, board: REPLAY_TYPE, name, nameColors: null,
    text: fighters.map(name => name.startsWith("@") ? name.toLowerCase() : name || "guest").join(" × "),
    when: row.recordedAt, replies: 0,
    file: { name: id + ".oskiedemo", type: REPLAY_TYPE, size: null,
      url: "https://aesthetic.computer/api/oskiewar-replays?id=" + encodeURIComponent(id) },
    origin: { name: "Oskiewar", url: named ? "https://oskiewar.com/" + encodeURIComponent(id.slice(3)) : url },
    media: { kind: "oskiewar", id, code: id, url, author: name, fighters,
      replayVersion: row.version, simulation: row.simulation, map: row.map, build: row.build,
      duration: row.tickRate > 0 ? row.durationTicks / row.tickRate : null },
  };
}

export async function replayPosts(db, limit) {
  const rows = await db.collection(REPLAY_COLLECTION).aggregate(replayCandidatePipeline(),
    { maxTimeMS: 10_000 }).toArray();
  return rows.filter(replayIsDense).map(replayPost).filter(Boolean)
    // Preserve active named players even when a newer bot farm fills the
    // archive. A handle is a preference here, never proof of activity.
    .sort((a, b) => Number(!!b.name) - Number(!!a.name))
    .slice(0, Math.max(0, Math.min(200, limit)));
}

export async function replayById(db, id) {
  if (!REPLAY_ID.test(id) || id.length > 80) return null;
  const row = await db.collection(REPLAY_COLLECTION).findOne({ ...REPLAY_VISIBLE, _id: id },
    { projection: REPLAY_FIELDS });
  return row ? replayPost(row) : null;
}
