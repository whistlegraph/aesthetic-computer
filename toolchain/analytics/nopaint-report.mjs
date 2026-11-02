#!/usr/bin/env node
// Read-only: run from /opt/ac/system with node --env-file=.env.
// No Paint → notepaint: clicks on No Paint's notepaint link, then what
// notepaint visits arriving from nopaint.art went on to do. Each half is its
// own aggregate; the two sites' visit IDs are never joined.
import { connect, closePool } from "../../system/backend/database.mjs";

const args = process.argv.slice(2);
const value = name => args[args.indexOf(name) + 1];
const hours = args.includes("--hours") ? Number(value("--hours")) : 24;
const end = args.includes("--end") ? new Date(value("--end")) : new Date();
if (!Number.isFinite(hours) || hours <= 0 || hours > 840 || !Number.isFinite(+end))
  throw new Error("Use --hours 1..840 and optional --end ISO-date");
const start = new Date(+end - hours * 3600000);
const count = field => ({ $sum: { $cond: [`$${field}`, 1, 0] } });
const both = (a, b) => ({ $sum: { $cond: [{ $and: [`$${a}`, `$${b}`] }, 1, 0] } });
const NOTEPAINT_ACTIONS = ["canvas_interacted", "painting_edited", "painting_signed", "painting_saved", "piece_made",
  "braincells_sheet_shown", "braincells_limit_reached", "braincells_checkout_opened", "braincells_purchased"];
const { db } = await connect();
try {
  const visits = db.collection("network-visits");
  const clicks = await visits.aggregate([
    { $match: { property: "nopaint.art", startedAt: { $gte: start, $lt: end }, linkVersion: 1 } },
    { $group: { _id: { automated: "$automated" }, visitsWithLink: { $sum: 1 },
      firstMeasuredVisit: { $min: "$startedAt" }, interacted: count("interacted"),
      notepaintOpened: count("actions.notepaint_opened"),
      // The sticker only shows once the title art is up; a visit that left
      // before it (or never interacted) never saw it. Click-through counts
      // only visits it was shown to.
      adShown: count("actions.notepaint_ad_shown"),
      openedAfterShown: both("actions.notepaint_ad_shown", "actions.notepaint_opened") } },
  ], { maxTimeMS: 10000 }).toArray();
  const arrivals = await visits.aggregate([
    { $match: { property: "notepaint.art", startedAt: { $gte: start, $lt: end } } },
    { $group: { _id: { automated: "$automated", from: { $switch: { branches: [
        { case: { $in: ["$referrerHost", ["nopaint.art", "www.nopaint.art"]] }, then: "nopaint.art" },
        { case: { $in: ["$referrerHost", ["notepaint.art", "www.notepaint.art"]] }, then: "notepaint.art (itself)" },
        { case: { $eq: [{ $ifNull: ["$referrerHost", null] }, null] }, then: "direct or unknown" },
      ], default: "elsewhere" } } },
      visits: { $sum: 1 }, interacted: count("interacted"), engaged: count("engaged"),
      ...Object.fromEntries(NOTEPAINT_ACTIONS.map(action => [action, count(`actions.${action}`)])) } },
    { $sort: { visits: -1 } },
  ], { maxTimeMS: 10000 }).toArray();
  const human = clicks.find(row => !row._id.automated);
  const fromNopaint = arrivals.find(row => !row._id.automated && row._id.from === "nopaint.art");
  // The Braincells funnel across every human notepaint arrival. The purchase
  // flag is set only after AC confirmed the payment with Stripe and granted it.
  const funnel = Object.fromEntries(["visits", "braincells_sheet_shown", "braincells_limit_reached",
    "braincells_checkout_opened", "braincells_purchased"].map(key => [key,
    arrivals.filter(row => !row._id.automated).reduce((sum, row) => sum + (row[key] || 0), 0)]));
  const rate = (n, d) => d ? +(100 * n / d).toFixed(2) : null;
  console.log(JSON.stringify({ start, end,
    unit: "page visits, not unique people; actions are once per visit",
    note: "Click-through is visits that opened notepaint among visits the sticker was shown to (notepaint_ad_shown, recorded at the visit's first interaction); visits before that action shipped are excluded, not zero. The second rate keeps the older denominator, every visit whose page carried the link. Arrivals group notepaint visits by referrer, which privacy settings can omit, so nopaint.art arrivals are a floor. A notepaint visit spans in-page navigation, so its actions belong to that arrival. The halves are separate aggregates, not joined journeys.",
    nopaintClickThroughPercent: rate(human?.openedAfterShown, human?.adShown),
    nopaintClickThroughOfAllLinkVisitsPercent: rate(human?.notepaintOpened, human?.visitsWithLink),
    fromNopaintMakingPercent: rate(fromNopaint?.painting_edited, fromNopaint?.visits),
    braincellsFunnel: funnel,
    nopaint: clicks,
    notepaintArrivals: arrivals,
  }, null, 2));
} finally { await closePool(); }
