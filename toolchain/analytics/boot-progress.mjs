#!/usr/bin/env node
// How far unfinished boots get. A boot that never reaches status "success"
// still leaves its event ladder behind, so the last rung says where it
// stopped: a closed tab, a login redirect, a stalled piece fetch, or a
// crawler that left after a second. Read-only.
//
//   node --env-file=system/.env toolchain/analytics/boot-progress.mjs [hours]
//
// Reads the same `boots` collection as the audience report and splits it the
// same way, so the counts line up with `journey-report.mjs audience`.
import { connect, closePool } from "../../system/backend/database.mjs";
import { visitProperty, automatedVisit } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";
import { botFamily } from "./boot-audience.mjs";

const hours = Number(process.argv[2] || 24);
if (!Number.isFinite(hours) || hours <= 0 || hours > 840) throw new Error("hours must be between 0 and 840");

// One rung name for every boot that stopped in the same place: timings and
// piece names are stripped so "fetching: a" and "fetching: b" fold together.
const rung = message => String(message || "(no events)")
  .replace(/(fetching|parsing|booting|loading disk|loaded): .*/, "$1: …")
  .replace(/\d+(\.\d+)?(ms|s)?/g, "N").slice(0, 60);
const median = list => { const sorted = list.filter(n => n != null).sort((a, b) => a - b); return sorted.length ? Math.round(sorted[sorted.length >> 1]) : null; };
const last = boot => (boot.events || []).at(-1);

// Where the unfinished boots of one audience stopped, most common rung first.
function ladder(boots) {
  const unfinished = boots.filter(boot => boot.status !== "success"), rungs = new Map();
  for (const boot of unfinished) {
    const key = rung(last(boot)?.message);
    rungs.set(key, [...(rungs.get(key) || []), boot]);
  }
  return {
    boots: boots.length, unfinished: unfinished.length,
    stoppedAt: [...rungs].sort((a, b) => b[1].length - a[1].length).map(([lastEvent, group]) => ({
      lastEvent, boots: group.length,
      medianElapsedMs: median(group.map(boot => last(boot)?.elapsed)),
      medianEvents: median(group.map(boot => (boot.events || []).length)),
    })),
  };
}

// One line per unfinished boot, oldest first; the handle is public, the rest
// is what the audience report already folds.
const line = ({ createdAt, status, server = {}, meta = {}, events = [] }) => ({
  at: createdAt.toISOString(), status, country: server.country || null, browser: meta.browser || null,
  mobile: !!meta.mobile, handle: meta.user?.handle ? `@${meta.user.handle}` : null,
  page: `${meta.host}${meta.path || ""}`, events: events.length,
  elapsedMs: events.at(-1)?.elapsed != null ? Math.round(events.at(-1).elapsed) : null,
  lastEvent: events.at(-1)?.message || null,
  error: events.find(event => event.level === "error")?.message || null,
});

const { db } = await connect();
try {
  const end = new Date(), start = new Date(+end - hours * 3600000);
  const boots = await db.collection("boots").find({ createdAt: { $gte: start, $lt: end } }, {
    projection: { _id: 0, status: 1, createdAt: 1, events: 1, "server.country": 1, "meta.host": 1, "meta.path": 1, "meta.search": 1,
      "meta.userAgent": 1, "meta.browser": 1, "meta.mobile": 1, "meta.embedded": 1, "meta.packMode": 1, "meta.localDev": 1,
      "meta.webdriver": 1, "meta.automation": 1, "meta.user.handle": 1 }, maxTimeMS: 30000,
  }).sort({ createdAt: 1 }).limit(50000).toArray();
  const humans = [], automated = [];
  for (const boot of boots) {
    const meta = boot.meta || {};
    if (!visitProperty(meta.host) || meta.localDev || meta.packMode || meta.embedded) continue;
    (automatedVisit({ userAgent: meta.userAgent, webdriver: meta.webdriver }, meta.search || "", meta.automation) ? automated : humans).push(boot);
  }
  const families = {};
  for (const boot of automated.filter(boot => boot.status !== "success")) {
    const family = botFamily(boot.meta?.userAgent, boot.meta?.search);
    families[family] = (families[family] || 0) + 1;
  }
  console.log(JSON.stringify({ start, end,
    unit: "page loads (boots) that never reached status success, by the last event they logged. A full ladder ending in \"running boot\" is a finished load whose success stamp never landed. Humans here are whatever the audience report calls human, so unlabeled crawlers show up as runs of old browser versions walking a list of paths.",
    humans: { ...ladder(humans), finishedMedianElapsedMs: median(humans.filter(boot => boot.status === "success").map(boot => last(boot)?.elapsed)),
      rows: humans.filter(boot => boot.status !== "success").map(line) },
    automated: { ...ladder(automated), unfinishedByFamily: families },
  }, null, 2));
} finally { await closePool?.(); }
