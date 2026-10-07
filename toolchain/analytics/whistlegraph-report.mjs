#!/usr/bin/env node
// Read-only: run from /opt/ac/system with node --env-file=.env.
import { connect, closePool } from "../../system/backend/database.mjs";

const args = process.argv.slice(2);
const value = name => args[args.indexOf(name) + 1];
const hours = args.includes("--hours") ? Number(value("--hours")) : 24;
const end = args.includes("--end") ? new Date(value("--end")) : new Date();
if (!Number.isFinite(hours) || hours <= 0 || hours > 840 || !Number.isFinite(+end))
  throw new Error("Use --hours 1..840 and optional --end ISO-date");
const start = new Date(+end - hours * 3600000);
const { db } = await connect();
try {
  const rows = await db.collection("network-visits").aggregate([
    { $match: { property: { $in: ["whistlegraph.org", "whistlegraph.app"] },
      startedAt: { $gte: start, $lt: end }, linkVersion: 1 } },
    { $group: { _id: { property: "$property", automated: "$automated" },
      visitsWithLink: { $sum: 1 }, firstMeasuredVisit: { $min: "$startedAt" },
      appClickVisits: { $sum: { $cond: ["$actions.whistlegraph_app_clicked", 1, 0] } },
      accessClickVisits: { $sum: { $cond: ["$actions.whistlegraph_access_clicked", 1, 0] } },
      referredFromOrg: { $sum: { $cond: [{ $in: ["$referrerHost", ["whistlegraph.org", "tv.whistlegraph.org"]] }, 1, 0] } },
      referredAccessClickVisits: { $sum: { $cond: [{ $and: [
        { $in: ["$referrerHost", ["whistlegraph.org", "tv.whistlegraph.org"]] },
        { $eq: ["$actions.whistlegraph_access_clicked", true] },
      ] }, 1, 0] } },
    } },
  ], { maxTimeMS: 10000 }).toArray();
  const org = rows.find(row => row._id.property === "whistlegraph.org" && !row._id.automated);
  console.log(JSON.stringify({ start, end,
    unit: "page visits with a reviewed link present and link tracking v1; not unique people",
    note: "Older/unmeasured visits are excluded, not zero clicks. Clicks are once per visit. Referrer arrivals are a separate aggregate, not joined journeys. Access clicks open a mail draft; they do not prove sending or signup.",
    orgClickThroughPercent: org?.visitsWithLink ? +(100 * org.appClickVisits / org.visitsWithLink).toFixed(2) : null,
    audience: rows.filter(row => !row._id.automated),
    automation: rows.filter(row => row._id.automated),
  }, null, 2));
} finally { await closePool(); }
