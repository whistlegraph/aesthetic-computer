// Read-only underperformer report, plus a guarded recorder for deletions that
// were already completed by a human in Instagram's own activity tool.

import { writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { ledgerPath, readLedger } from "./publish.mjs";

const HOUR = 60 * 60 * 1000;
const DAY = 24 * HOUR;

export const trimPolicy = Object.freeze({
  maxViews: 500,
  minAgeHours: 24,
  lookbackDays: 30,
});

export const deletedReason =
  "underperformer trim (bulk delete via web Your activity tool, 30-day pool)";

export function trimCandidates(posts, {
  now = Date.now(),
  maxViews = trimPolicy.maxViews,
  minAgeHours = trimPolicy.minAgeHours,
  lookbackDays = trimPolicy.lookbackDays,
} = {}) {
  const earliest = now - lookbackDays * DAY;
  const latest = now - minAgeHours * HOUR;

  return posts.filter((post) => {
    const publishedAt = Date.parse(post.publishedAt);
    const views = post.insights?.views;
    return post.mode === "live" && post.mediaId && !post.deletedAt &&
      Number.isFinite(publishedAt) && publishedAt >= earliest &&
      publishedAt <= latest && Number.isFinite(views) && views < maxViews;
  }).sort((a, b) => Date.parse(a.publishedAt) - Date.parse(b.publishedAt));
}

export function recordDeleted(ledger, mediaIds, {
  confirmed = false,
  at = new Date().toISOString(),
  policy = {},
} = {}) {
  if (!confirmed)
    throw new Error("refusing to write without --confirmed-web-delete");
  if (!Array.isArray(ledger?.posts)) throw new Error("ledger posts are missing");

  const selected = [...new Set(mediaIds.map(String).filter(Boolean))];
  if (!selected.length) throw new Error("at least one media id is required");
  const now = Date.parse(at);
  if (!Number.isFinite(now)) throw new Error("deletion timestamp is invalid");

  const eligible = new Map(trimCandidates(ledger.posts, { now, ...policy })
    .map((post) => [String(post.mediaId), post]));
  const refused = selected.filter((mediaId) => !eligible.has(mediaId));
  if (refused.length)
    throw new Error(`not eligible for trim: ${refused.join(", ")}`);

  const chosen = new Set(selected);
  return {
    ...ledger,
    posts: ledger.posts.map((post) => chosen.has(String(post.mediaId)) ? {
      ...post,
      deletedAt: at,
      deletedReason,
    } : post),
  };
}

export function reportFor(ledger, options = {}) {
  const now = options.now ?? Date.now();
  return trimCandidates(ledger.posts || [], { ...options, now }).map((post) => ({
    id: post.id,
    mediaId: String(post.mediaId),
    publishedAt: post.publishedAt,
    ageHours: Math.floor((now - Date.parse(post.publishedAt)) / HOUR),
    views: post.insights.views,
    avgWatchMs: post.insights.ig_reels_avg_watch_time ?? null,
    skipRate: post.insights.reels_skip_rate ?? null,
  }));
}

function valueAfter(args, flag) {
  const index = args.indexOf(flag);
  return index < 0 ? null : args[index + 1];
}

function printReport(rows) {
  console.log("oskiewar trim · read-only");
  console.log("<500 views · 24h–30d old · missing insights protected");
  console.log(`${rows.length} candidate${rows.length === 1 ? "" : "s"}`);
  for (const row of rows) {
    const watch = row.avgWatchMs == null ? "—" : `${row.avgWatchMs}ms`;
    const skip = row.skipRate == null ? "—" : `${row.skipRate}%`;
    console.log(`${row.publishedAt} · ${row.views} views · ${watch} watch · ` +
      `${skip} skip · ${row.id} · ${row.mediaId}`);
  }
  console.log("No Instagram action taken.");
}

export function run(args = process.argv.slice(2)) {
  const ledger = readLedger();
  const mediaList = valueAfter(args, "--record-deleted");

  if (mediaList != null) {
    const mediaIds = mediaList.split(",").map((id) => id.trim()).filter(Boolean);
    const next = recordDeleted(ledger, mediaIds, {
      confirmed: args.includes("--confirmed-web-delete"),
    });
    writeFileSync(ledgerPath, JSON.stringify(next, null, 2) + "\n");
    console.log(`recorded ${mediaIds.length} deleted reel${mediaIds.length === 1 ? "" : "s"}`);
    return;
  }

  const rows = reportFor(ledger);
  if (args.includes("--json")) console.log(JSON.stringify({ policy: trimPolicy, rows }, null, 2));
  else printReport(rows);
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) run();
