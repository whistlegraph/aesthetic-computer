// Read-only companion to anonymous fish. Account events have their own aliases;
// never join their subjects/sessions to network-visits or the anonymous fish.
import { createHash } from "node:crypto";
import { FEED_ACTIONS, FEED_ENTRIES, FEED_EXITS, FEED_CONTENT } from "../../system/public/aesthetic.computer/lib/feed-activity.mjs";
import { fisherySnapshot } from "./human-fishery.mjs";

export function mimeFisheryQueries(options) {
  const since = new Date(Math.max(+options.since, +(options.match.startedAt?.$gte || 0)));
  const between = { $gte: since, $lte: options.observedAt };
  return {
    events: { tenant: "aesthetic", property: options.match.property, piece: "mime", activityVersion: 1,
      action: { $in: FEED_ACTIONS }, at: between },
    arrivals: { property: { $in: options.match.property.$in.filter(property => property === "mime.ac") },
      automated: false, startedAt: between },
  };
}

export function mimeFisherySnapshot(events, arrivals, names, options) {
  const short = value => createHash("sha256").update(String(value)).digest("hex").slice(0, 16);
  const valid = events.filter(row => row.tenant === "aesthetic" && row.piece === "mime" && row.activityVersion === 1 &&
    FEED_ACTIONS.includes(row.action) && FEED_ENTRIES.includes(row.entry));
  return {
    source: "account-activity + network-visits (separate instruments)",
    note: "Account events identify server-verified signed-in accounts, not necessarily people. Direct page arrivals have no account identity and can overlap account events: do not add the counts or join them. Anonymous prompt swipe-ins are not covered. focusedMs is an incremental foreground interval, not proof of attention or current presence. Deduplicate account events by event across polls; temporary fish names change at UTC midnight. startedAfter bounds account event receipt times and direct-page arrival times.",
    eventsTruncated: events.length > options.limit,
    arrivalsTruncated: arrivals.length > options.limit,
    events: valid.slice(0, options.limit).map(row => ({
      event: short(`mime-event:${row._id}`),
      account: names.get(String(row.user)) ? `@${names.get(String(row.user)).replace(/^@/, "")}` : `account-${short(`aesthetic:${row.user}`)}`,
      session: short(`aesthetic:${row.user}:${row.session}`), sequence: row.sequence,
      at: row.at, property: row.property, action: row.action, entry: row.entry,
      ...(row.action === "mime_piece_viewed" && FEED_CONTENT.test(row.contentId || "") &&
          Number.isSafeInteger(row.focusedMs) && row.focusedMs > 0 && row.focusedMs <= 30000
        ? { contentId: row.contentId, focusedMs: row.focusedMs } : {}),
      ...(row.action === "mime_closed" && FEED_EXITS.includes(row.exitReason) ? { exitReason: row.exitReason } : {}),
    })),
    directPageArrivals: fisherySnapshot(arrivals, options).fish,
  };
}

export async function mimeFishery(db, options) {
  const query = mimeFisheryQueries(options);
  const [events, arrivals] = await Promise.all([
    db.collection("account-activity").find(query.events, { projection: { tenant: 1, piece: 1, activityVersion: 1,
      user: 1, session: 1, sequence: 1, at: 1, property: 1, action: 1, entry: 1, contentId: 1, focusedMs: 1, exitReason: 1 }, maxTimeMS: 10000 })
      .sort({ at: -1, _id: -1 }).limit(options.limit + 1).toArray(),
    db.collection("network-visits").find(query.arrivals, { projection: { property: 1, surface: 1, startedAt: 1,
      lastSeenAt: 1, activeSeconds: 1, engaged: 1, actions: 1 }, maxTimeMS: 10000 })
      .sort({ startedAt: -1, _id: -1 }).limit(options.limit + 1).toArray(),
  ]);
  const users = [...new Set(events.slice(0, options.limit).map(row => row.user))];
  const names = new Map((users.length ? await db.collection("@handles").find({ _id: { $in: users } },
    { projection: { handle: 1 }, maxTimeMS: 10000 }).toArray() : []).map(row => [String(row._id), row.handle]));
  return mimeFisherySnapshot(events, arrivals, names, options);
}
