// Metrics Daily, 2026.09.28
// Folds each finished UTC day of web visits, direct downloads and app opens
// into one `metrics-daily` row, keyed by the day. The raw collections expire
// after 35 days; these rows keep only counts, so they stay. A day is written
// once, from lith's hourly runner, and missing days are backfilled for as far
// back as the raw data still reaches. App Store numbers live with Apple and
// are read by toolchain/mcp/analytics-mcp.mjs, not here.

export const METRICS_DAILY_COLLECTION = "metrics-daily";
const DAY = 86400000;
const BACKFILL_DAYS = 34;
const FIRST_DAY = "2026-09-23"; // The visit collector began here.

const tally = (rows, fields) => Object.fromEntries(rows.map(row => [row._id,
  Object.fromEntries(fields.map(field => [field, row[field]]))]));

async function foldDay(db, start) {
  const end = new Date(+start + DAY), day = start.toISOString().slice(0, 10);
  const flag = field => ({ $sum: { $cond: [`$${field}`, 1, 0] } });

  const visits = await db.collection("network-visits").aggregate([
    { $match: { startedAt: { $gte: start, $lt: end } } },
    { $group: { _id: "$property", visits: { $sum: { $cond: ["$automated", 0, 1] } },
      interacted: { $sum: { $cond: [{ $and: ["$interacted", { $not: ["$automated"] }] }, 1, 0] } },
      engaged: { $sum: { $cond: [{ $and: ["$engaged", { $not: ["$automated"] }] }, 1, 0] } },
      automated: flag("automated") } },
  ], { maxTimeMS: 20000 }).toArray();

  const downloads = await db.collection("downloads").aggregate([
    { $match: { at: { $gte: start, $lt: end } } },
    { $group: { _id: "$app", downloads: { $sum: { $cond: ["$automated", 0, 1] } },
      hashes: { $addToSet: { $cond: ["$automated", null, "$hash"] } }, automated: flag("automated") } },
    { $set: { places: { $size: { $setDifference: ["$hashes", [null]] } } } },
  ], { maxTimeMS: 20000 }).toArray();

  const opens = await db.collection("app-opens").aggregate([
    { $match: { day } },
    { $group: { _id: "$app", active: { $sum: 1 }, opens: { $sum: "$opens" }, fresh: flag("fresh") } },
  ], { maxTimeMS: 20000 }).toArray();

  return { day, generatedAt: new Date(),
    visits: tally(visits, ["visits", "interacted", "engaged", "automated"]),
    downloads: tally(downloads, ["downloads", "places", "automated"]),
    opens: tally(opens, ["active", "opens", "fresh"]) };
}

// Writes every finished day in the backfill window that has no row yet.
export async function rollupMissingDays(db, now = new Date()) {
  const collection = db.collection(METRICS_DAILY_COLLECTION);
  const today = new Date(now.toISOString().slice(0, 10) + "T00:00:00Z");
  const have = new Set(await collection.distinct("_id"));
  const written = [];
  for (let back = BACKFILL_DAYS; back >= 1; back--) {
    const start = new Date(+today - back * DAY), day = start.toISOString().slice(0, 10);
    if (day < FIRST_DAY || have.has(day)) continue;
    await collection.updateOne({ _id: day }, { $setOnInsert: await foldDay(db, start) }, { upsert: true });
    written.push(day);
  }
  return written;
}
