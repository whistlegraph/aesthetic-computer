#!/usr/bin/env node
// Private CLI transport: SSH/MCP only, no public account-activity read endpoint.
import { createHash } from "node:crypto";
import { connect, closePool } from "../../system/backend/database.mjs";
import { visitScopeMatch, visitReferrer, visitProperty, visitSurface, automatedVisit } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";
import { laklokFeatureReport } from "./laklok-feature-report.mjs";

const mode = process.argv[2];
const { hours = 24, limit = mode === "features" ? 30 : 100, handle, scope = "studio", property } = JSON.parse(process.argv[3] || "{}");
if (!["accounts", "referrers", "features"].includes(mode) || !Number.isFinite(hours) || hours <= 0 || hours > 840 ||
    !Number.isInteger(limit) || limit < 1 || limit > (mode === "features" ? 50 : 500) ||
    (handle !== undefined && !/^@?[a-z0-9_-]{1,64}$/i.test(handle))) throw new Error("Invalid report options");
const scoped = visitScopeMatch(scope), end = new Date(), start = new Date(+end - hours * 3600000);
if (property !== undefined) {
  const canonical = visitProperty(property);
  if (!canonical || !scoped.property.$in.includes(canonical)) throw new Error("Property is outside the selected scope");
  scoped.property.$in = [canonical];
}
const { db } = await connect();
try {
  if (mode === "features") {
    console.log(JSON.stringify(await laklokFeatureReport(db, { start, end, scoped, handle, limit })));
  } else if (mode === "accounts") {
    const query = { at: { $gte: start, $lt: end }, ...scoped };
    if (handle) {
      const handles = await db.collection("@handles").find({ handle: handle.replace(/^@/, "") }, { projection: { _id: 1 } }).limit(10).toArray();
      query.$or = handles.map(row => String(row._id).startsWith("sotce-")
        ? { tenant: "sotce", user: String(row._id).slice(6) }
        : { tenant: "aesthetic", user: String(row._id) });
      if (!query.$or.length) query.$or = [{ user: { $in: [] } }];
    }
    const collection = db.collection("account-activity");
    const [totals = { accounts: 0, events: 0 }] = await collection.aggregate([
      { $match: query },
      { $group: { _id: { tenant: "$tenant", user: "$user" }, events: { $sum: 1 } } },
      { $group: { _id: null, accounts: { $sum: 1 }, events: { $sum: "$events" } } },
      { $project: { _id: 0, accounts: 1, events: 1 } },
    ], { maxTimeMS: 10000 }).toArray();
    const rows = await collection.find(query, {
      projection: { _id: 0, tenant: 1, user: 1, session: 1, sequence: 1, at: 1, property: 1, piece: 1, action: 1, referrerHost: 1 }, maxTimeMS: 10000,
    }).sort({ at: -1, _id: -1 }).limit(limit + 1).toArray();
    const key = row => row.tenant === "sotce" ? `sotce-${row.user}` : row.user;
    const names = new Map((await db.collection("@handles").find({ _id: { $in: [...new Set(rows.map(key))] } }, { projection: { handle: 1 } }).toArray()).map(row => [String(row._id), row.handle]));
    const short = value => createHash("sha256").update(value).digest("hex").slice(0, 12);
    console.log(JSON.stringify({ start, end, scope, totals, truncated: rows.length > limit,
      identity: "server-verified account; authenticated activity is not proof of a human",
      note: "Only activity recorded after deployment is available. Session aliases group one runtime login, not separate anonymous visits. Referrers are browser-reported sites; null means direct or unavailable.",
      events: rows.slice(0, limit).reverse().map(row => ({ at: row.at,
        account: names.get(key(row)) ? `@${names.get(key(row))}` : `account-${short(key(row))}`,
        tenant: row.tenant, session: short(`${key(row)}:${row.session}`), sequence: row.sequence, property: row.property,
        piece: row.piece, action: row.action, referrerHost: row.referrerHost,
      })),
    }));
  } else {
    const visits = await db.collection("network-visits").aggregate([
      { $match: { ...scoped, automated: false, startedAt: { $gte: start, $lt: end }, referrerHost: { $exists: true } } },
      { $group: { _id: { property: "$property", referrerHost: "$referrerHost" }, visits: { $sum: 1 },
        interacted: { $sum: { $cond: ["$interacted", 1, 0] } }, engaged: { $sum: { $cond: ["$engaged", 1, 0] } } } },
      { $sort: { visits: -1 } }, { $limit: limit + 1 },
    ], { maxTimeMS: 10000 }).toArray();
    // Existing boot logs provide historical referral context for the AC shell.
    // Do not add these totals to visits: the instruments measure different things.
    const boots = await db.collection("boots").find({ createdAt: { $gte: start, $lt: end } }, {
      projection: { _id: 0, "meta.host": 1, "meta.path": 1, "meta.referrer": 1, "meta.userAgent": 1, "meta.embedded": 1, "meta.packMode": 1, "meta.localDev": 1 }, maxTimeMS: 10000,
    }).sort({ createdAt: -1 }).limit(10001).toArray();
    const groups = new Map();
    for (const { meta = {} } of boots.slice(0, 10000)) {
      const property = visitProperty(meta.host);
      if (!scoped.property.$in.includes(property) || visitSurface(meta.path) === null || meta.embedded || meta.packMode || meta.localDev || automatedVisit({ userAgent: meta.userAgent })) continue;
      const referrerHost = visitReferrer(meta.referrer), key = JSON.stringify([property, referrerHost]);
      const row = groups.get(key) || { property, referrerHost, boots: 0 };
      row.boots++; groups.set(key, row);
    }
    console.log(JSON.stringify({ start, end, scope,
      note: "Referrer sites only. Null means direct or unavailable, not proof of direct traffic. Browser privacy, apps and redirects can omit referrers. Visit and legacy boot totals must not be added together. No account or visitor identity is inferred from a referrer.",
      visits: visits.slice(0, limit).map(row => ({ ...row._id, visits: row.visits, interacted: row.interacted, engaged: row.engaged })),
      visitsTruncated: visits.length > limit,
      legacyBoots: [...groups.values()].sort((a, b) => b.boots - a.boots).slice(0, limit),
      legacyBootsTruncated: boots.length > 10000 || groups.size > limit,
    }));
  }
} finally { await closePool(); }
