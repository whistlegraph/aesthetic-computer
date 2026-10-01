import { createHash } from "node:crypto";
import { LAKLOK_FEATURES, LAKLOK_ACTIONS, LAKLOK_PIECES, LAKLOK_FEATURE_VERSION } from "../../system/public/aesthetic.computer/lib/laklok-activity.mjs";

export async function laklokFeatureReport(db, { start, end, scoped, handle, limit }) {
  const query = { ...scoped, at: { $gte: start, $lt: end }, tenant: "aesthetic",
    piece: { $in: LAKLOK_PIECES }, featureVersion: LAKLOK_FEATURE_VERSION,
    action: { $in: ["piece_opened", ...LAKLOK_ACTIONS] } };
  if (handle) {
    const rows = await db.collection("@handles").find({ handle: handle.replace(/^@/, "") }, { projection: { _id: 1 } }).limit(10).toArray();
    query.user = { $in: rows.map(row => String(row._id)).filter(id => !id.startsWith("sotce-")) };
  }
  const collection = db.collection("account-activity");
  const aggregate = pipeline => collection.aggregate(pipeline, { maxTimeMS: 10000 }).toArray();
  const rows = await aggregate([
    { $match: query },
    { $group: { _id: "$user", first: { $min: "$at" }, last: { $max: "$at" },
      pieces: { $addToSet: "$piece" }, opens: { $sum: { $cond: [{ $eq: ["$action", "piece_opened"] }, 1, 0] } } } },
    { $sort: { last: -1, _id: 1 } }, { $limit: limit + 1 },
  ]);
  const selected = rows.slice(0, limit), users = selected.map(row => row._id);
  const selectedQuery = { ...query, user: { $in: users } };
  const counts = await aggregate([
    { $match: { ...selectedQuery, action: { $in: LAKLOK_ACTIONS } } },
    { $group: { _id: { user: "$user", action: "$action" }, count: { $sum: 1 } } },
  ]);
  const daily = await aggregate([
    { $match: selectedQuery },
    { $group: { _id: { user: "$user", day: { $dateToString: { format: "%Y-%m-%d", date: "$at", timezone: "+00:00" } } },
      opens: { $sum: { $cond: [{ $eq: ["$action", "piece_opened"] }, 1, 0] } },
      uses: { $sum: { $cond: [{ $ne: ["$action", "piece_opened"] }, 1, 0] } } } },
    { $sort: { "_id.day": 1 } },
  ]);
  const topFeatures = await aggregate([
    { $match: { ...query, action: { $in: LAKLOK_ACTIONS } } },
    { $group: { _id: { user: "$user", action: "$action" }, count: { $sum: 1 } } },
    { $group: { _id: "$_id.action", count: { $sum: "$count" }, accounts: { $sum: 1 } } },
    { $sort: { count: -1, _id: 1 } },
  ]);
  const names = new Map((await db.collection("@handles").find({ _id: { $in: users } }, { projection: { handle: 1 } }).toArray()).map(row => [String(row._id), row.handle]));
  return { product: "laklok", start, end, featureVersion: LAKLOK_FEATURE_VERSION,
    note: "Recorded feature uses by authenticated account, not unique people. Only the instrumented rollout is included. Requests are not confirmed playback, message delivery or navigation. Zero means no recorded use of a supported feature during this window, not proof of non-use or exposure. Opt-outs, offline use and dropped events leave gaps. Daily buckets are UTC; absent days are unobserved. Top features cover all matching accounts; account detail is bounded.",
    topFeatures: topFeatures.map(row => ({ action: row._id, count: row.count, accounts: row.accounts })),
    truncated: rows.length > limit,
    accounts: selected.map(row => {
      const interfaces = [...new Set(row.pieces.map(piece => piece === "laklok-vector" ? "vector" : "raster"))];
      const supported = LAKLOK_ACTIONS.filter(action => LAKLOK_FEATURES[action].some(mode => interfaces.includes(mode)));
      const features = Object.fromEntries(supported.map(action => [action, 0]));
      for (const item of counts) if (item._id.user === row._id) features[item._id.action] = item.count;
      return { account: names.get(row._id) ? `@${names.get(row._id)}` : `account-${createHash("sha256").update(row._id).digest("hex").slice(0, 12)}`,
        first: row.first, last: row.last, interfaces, opens: row.opens, features,
        notRecorded: supported.filter(action => !features[action]),
        daily: daily.filter(item => item._id.user === row._id).map(item => ({ day: item._id.day, opens: item.opens, uses: item.uses })),
      };
    }),
  };
}
