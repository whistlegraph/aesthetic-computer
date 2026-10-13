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
  const rows = await db.collection(REPLAY_COLLECTION).find(REPLAY_VISIBLE,
    { projection: REPLAY_FIELDS }).sort({ recordedAt: -1, _id: -1 }).limit(limit).toArray();
  return rows.map(replayPost).filter(Boolean);
}

export async function replayById(db, id) {
  if (!REPLAY_ID.test(id) || id.length > 80) return null;
  const row = await db.collection(REPLAY_COLLECTION).findOne({ ...REPLAY_VISIBLE, _id: id },
    { projection: REPLAY_FIELDS });
  return row ? replayPost(row) : null;
}
