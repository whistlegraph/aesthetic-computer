// AC Human Fishery: bounded snapshots of anonymous visits with interaction.
import { createHash } from "node:crypto";
import { VISIT_ACTIONS, visitScopeMatch } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";

const COLORS = ["coral", "gold", "mint", "blue", "violet", "silver", "pink", "amber"];
const FISH = ["guppy", "tetra", "minnow", "koi", "trout", "perch", "goby", "tang"];

export function fisheryOptions({ minutes = 5, scope = "studio", limit = 50, startedAfter } = {}, now = new Date()) {
  if (!Number.isFinite(minutes) || minutes < 1 || minutes > 60) throw new Error("minutes must be 1..60");
  if (!Number.isInteger(limit) || limit < 1 || limit > 200) throw new Error("limit must be 1..200");
  const start = startedAfter === undefined ? null : new Date(startedAfter);
  if (start && (!Number.isFinite(+start) || +start > +now)) throw new Error("startedAfter must be a past ISO date");
  return { scope, limit, since: new Date(+now - minutes * 60000), observedAt: now,
    match: { ...visitScopeMatch(scope), automated: false, interacted: true,
      lastSeenAt: { $gte: new Date(+now - minutes * 60000), $lte: now },
      ...(start ? { startedAt: { $gte: start } } : {}) } };
}

export function fisherySnapshot(rows, options) {
  const day = options.observedAt.toISOString().slice(0, 10);
  return {
    name: "AC Human Fishery", source: "silo._firehose → network-visits", observedAt: options.observedAt, since: options.since,
    scope: options.scope, truncated: rows.length > options.limit,
    unit: "anonymous page visits with interaction; not unique people or continuous presence",
    note: "Uses Silo's existing throttled firehose. Fish names last for this visit and UTC day. Actions are cumulative flags, not an ordered path. Repeat after at least 15 seconds to see changes; sites and separate page visits are not linked.",
    fish: rows.slice(0, options.limit).map(row => {
      const hash = createHash("sha256").update(`${day}\0${row._id}`).digest();
      return { fish: `${COLORS[hash[0] % COLORS.length]}-${FISH[hash[1] % FISH.length]}-${hash.subarray(2, 6).toString("hex")}`,
        property: row.property, surface: row.surface, arrivedAt: row.startedAt,
        lastFirehoseAt: row.lastFirehoseAt,
        lastReportedAt: row.lastSeenAt, visibleSecondsAtLeast: row.activeSeconds,
        engaged: row.engaged === true, actions: VISIT_ACTIONS.filter(action => row.actions?.[action] === true) };
    }),
  };
}

export function fisheryPipeline(options) {
  return [
    { $match: { ns: "network-visits", time: { $gte: options.since, $lte: options.observedAt }, op: { $in: ["insert", "update", "replace"] } } },
    { $group: { _id: "$docId", lastFirehoseAt: { $max: "$time" } } },
    { $lookup: { from: "network-visits", localField: "_id", foreignField: "_id", pipeline: [
      { $match: options.match },
      { $project: { property: 1, surface: 1, startedAt: 1, lastSeenAt: 1, activeSeconds: 1, engaged: 1, actions: 1 } },
    ], as: "visit" } },
    { $unwind: "$visit" },
    { $replaceWith: { $mergeObjects: ["$visit", { lastFirehoseAt: "$lastFirehoseAt" }] } },
    { $sort: { lastFirehoseAt: -1, _id: 1 } },
    { $limit: options.limit + 1 },
  ];
}
