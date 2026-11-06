#!/usr/bin/env node
// Private CLI transport: SSH/MCP only, no public account-activity read endpoint.
import { createHash } from "node:crypto";
import { connect, closePool } from "../../system/backend/database.mjs";
import { visitScopeMatch, visitReferrer, visitProperty, visitSurface, automatedVisit, aeselVisitReportPipeline } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";
import { laklokFeatureReport } from "./laklok-feature-report.mjs";
import { mimeJourneyReport } from "./mime-journey-report.mjs";
import { foldPieceLandings } from "./piece-landings.mjs";
import { foldBootAudience, foldHandleUsage } from "./boot-audience.mjs";

const mode = process.argv[2];
const { hours = 24, limit = mode === "features" ? 30 : 100, handle, scope = "studio", property } = JSON.parse(process.argv[3] || "{}");
if (!["accounts", "referrers", "features", "pieces", "mime", "audience", "handles"].includes(mode) || !Number.isFinite(hours) || hours <= 0 || hours > 840 ||
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
  if (mode === "mime") {
    console.log(JSON.stringify(await mimeJourneyReport(db, { start, end, scoped, handle, limit })));
  } else if (mode === "audience" || mode === "handles") {
    // Boots carry the user agent, query flags, Cloudflare country and handle
    // that the visit tracker deliberately drops, so the nature of the traffic
    // (which bots, which browsers, which countries, which accounts) reads here.
    const boots = await db.collection("boots").find({ createdAt: { $gte: start, $lt: end } }, {
      projection: { _id: 0, status: 1, createdAt: 1, "server.country": 1, "meta.host": 1, "meta.path": 1, "meta.search": 1, "meta.referrer": 1,
        "meta.userAgent": 1, "meta.browser": 1, "meta.mobile": 1, "meta.language": 1, "meta.timezone": 1, "meta.embedded": 1,
        "meta.packMode": 1, "meta.localDev": 1, "meta.webdriver": 1, "meta.automation": 1, "meta.user.handle": 1 }, maxTimeMS: 15000,
    }).sort({ createdAt: -1 }).limit(50001).toArray();
    const properties = scoped.property.$in;
    if (mode === "audience") {
      console.log(JSON.stringify({ start, end, scope,
        unit: "page loads (boots), split into humans, automated and walls (embedded iframes such as the kidlisp.com gallery screens). A boot exists only when boot.mjs ran, so bots that never execute JavaScript (curl, python, most scrapers) are invisible here; Cloudflare sees those. The visit tracker's automated count is a different instrument and must not be summed with this one. Country is Cloudflare's; (unknown) means the header was absent. Signed-in is not proof of a human.",
        ...foldBootAudience(boots.slice(0, 50000), { properties, limit: Math.min(limit, 25) }),
        bootsTruncated: boots.length > 50000,
      }));
    } else {
      // Server-verified account activity (piece opens, action milestones) is a
      // second instrument for the same handles; it starts at its own rollout.
      const activity = await db.collection("account-activity").aggregate([
        { $match: { at: { $gte: start, $lt: end }, ...scoped } },
        { $group: { _id: { tenant: "$tenant", user: "$user" }, events: { $sum: 1 },
          sessions: { $addToSet: "$session" }, pieces: { $addToSet: "$piece" }, actions: { $addToSet: "$action" }, lastAt: { $max: "$at" } } },
        { $sort: { events: -1 } }, { $limit: 500 },
      ], { maxTimeMS: 10000 }).toArray();
      const key = row => row._id.tenant === "sotce" ? `sotce-${row._id.user}` : row._id.user;
      const names = new Map((await db.collection("@handles").find({ _id: { $in: activity.map(key) } }, { projection: { handle: 1 } }).toArray())
        .map(row => [String(row._id), row.handle]));
      const folded = foldHandleUsage(boots.slice(0, 50000), { properties, limit });
      const byHandle = new Map(folded.rows.map(row => [row.handle, row]));
      let unnamed = 0;
      for (const row of activity) {
        const handle = names.get(key(row));
        if (!handle) { unnamed++; continue; }
        const name = `@${handle}`;
        const target = byHandle.get(name) || (byHandle.size < limit ? { handle: name, boots: 0, completed: 0, mobile: 0, properties: [], pieces: [], browsers: [], firstSeen: null, lastSeen: null } : null);
        if (!target) continue;
        target.activity = { events: row.events, sessions: row.sessions.length, pieces: row.pieces.filter(Boolean).slice(0, 12), actions: row.actions.filter(Boolean).slice(0, 12), lastAt: row.lastAt };
        byHandle.set(name, target);
      }
      const rows = [...byHandle.values()].sort((a, b) => (b.boots + (b.activity?.events || 0)) - (a.boots + (a.activity?.events || 0)));
      console.log(JSON.stringify({ start, end, scope,
        unit: "signed-in usage per public handle. boots = page loads carrying that handle (a tab kept open all day is one boot); activity = server-verified account-activity events (piece opens and action milestones) from that rollout onward. Bots, walls and local dev are excluded. Login is not proof of a human; absence here is not proof of absence.",
        handles: folded.handles, anonymousBoots: folded.anonymousBoots, activityAccounts: activity.length, activityWithoutHandle: unnamed,
        truncated: folded.truncated || activity.length >= 500, rows, bootsTruncated: boots.length > 50000,
      }));
    }
  } else if (mode === "pieces") {
    const boots = await db.collection("boots").find({ createdAt: { $gte: start, $lt: end } }, {
      projection: { _id: 0, status: 1, "meta.host": 1, "meta.path": 1, "meta.search": 1, "meta.referrer": 1, "meta.userAgent": 1, "meta.embedded": 1,
        "meta.packMode": 1, "meta.localDev": 1, "meta.webdriver": 1, "meta.automation": 1, "meta.user": 1 }, maxTimeMS: 15000,
    }).sort({ createdAt: -1 }).limit(50001).toArray();
    console.log(JSON.stringify({ start, end, scope,
      unit: "page loads (boots) per landing piece; anonymous and signed-in. Moving between pieces inside a tab is not a new boot. completed = the boot finished loading.",
      ...foldPieceLandings(boots.slice(0, 50000), { properties: scoped.property.$in, limit }),
      bootsTruncated: boots.length > 50000,
    }));
  } else if (mode === "features") {
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
      projection: { _id: 0, tenant: 1, user: 1, session: 1, sequence: 1, at: 1, property: 1, piece: 1, action: 1, referrerHost: 1, activityVersion: 1, entry: 1, contentId: 1, focusedMs: 1, exitReason: 1 }, maxTimeMS: 10000,
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
        ...(row.activityVersion === 1 ? { entry: row.entry, contentId: row.contentId, focusedMs: row.focusedMs, exitReason: row.exitReason } : {}),
      })),
    }));
  } else {
    const aesel = scoped.property.$in.includes("aesel.app")
      ? await db.collection("network-visits").aggregate(aeselVisitReportPipeline(start, end), { maxTimeMS: 10000 }).toArray() : [];
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
      aesel,
      visits: visits.slice(0, limit).map(row => ({ ...row._id, visits: row.visits, interacted: row.interacted, engaged: row.engaged })),
      visitsTruncated: visits.length > limit,
      legacyBoots: [...groups.values()].sort((a, b) => b.boots - a.boots).slice(0, limit),
      legacyBootsTruncated: boots.length > 10000 || groups.size > limit,
    }));
  }
} finally { await closePool(); }
