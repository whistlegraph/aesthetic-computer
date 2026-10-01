// Cumulative iOS session snapshots. Retries update one row, never add an open.
export const NATIVE_USAGE_COLLECTION = "native-app-sessions";
const DAY = 86400000;
const UUID = /^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}$/i;
const VERSION = /^\d+(?:\.\d+){0,3}$/;

export function validateNativeUsage(body, now = new Date()) {
  if (!body || body.schema !== 1 || body.app !== "aestheticcomputer" ||
      !["ios", "ipados"].includes(body.platform) ||
      typeof body.version !== "string" || !VERSION.test(body.version) ||
      typeof body.build !== "string" || !/^\d{1,9}$/.test(body.build) ||
      typeof body.install !== "string" || !UUID.test(body.install) ||
      typeof body.session !== "string" || !UUID.test(body.session) ||
      typeof body.startedAt !== "string" || !/^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\dZ$/.test(body.startedAt) ||
      !Number.isInteger(body.activeSeconds) || body.activeSeconds < 0 || body.activeSeconds > 86400 ||
      ![body.firstObserved, body.ready, body.interacted].every(x => typeof x === "boolean")) return null;
  const startedAt = new Date(body.startedAt);
  if (!Number.isFinite(+startedAt) || +startedAt < +now - 7 * DAY || +startedAt > +now + 60000 ||
      body.activeSeconds > Math.max(0, (+now - +startedAt) / 1000) + 60) return null;
  return { schema: 1, app: body.app, platform: body.platform, version: body.version, build: body.build,
    install: body.install.toLowerCase(), session: body.session.toLowerCase(), startedAt,
    activeSeconds: body.activeSeconds, firstObserved: body.firstObserved, ready: body.ready, interacted: body.interacted };
}

export function nativeUsageWrite(value, now = new Date()) {
  const { app, install, session, platform, version, build, startedAt, firstObserved } = value;
  return {
    id: `${app}:${install}:${session}`,
    update: {
      $setOnInsert: { app, install, session, platform, version, build, startedAt, firstObserved,
        day: startedAt.toISOString().slice(0, 10), expiresAt: new Date(+startedAt + 35 * DAY) },
      $max: { activeSeconds: value.activeSeconds, ready: value.ready, interacted: value.interacted, receivedAt: now },
    },
  };
}

// Sessions spanning midnight are attributed to the day they opened. Installation
// means first observed by this telemetry version, not an App Store download.
export async function nativeUsageReport(db, start, end = new Date()) {
  const sessions = await db.collection(NATIVE_USAGE_COLLECTION).aggregate([
    { $match: { startedAt: { $gte: start, $lt: end } } },
    { $group: { _id: { app: "$app", install: "$install", day: "$day" },
      opens: { $sum: 1 }, activeSeconds: { $sum: "$activeSeconds" },
      loaded: { $sum: { $cond: ["$ready", 1, 0] } },
      interacted: { $sum: { $cond: ["$interacted", 1, 0] } },
      engaged: { $sum: { $cond: [{ $and: ["$ready", "$interacted", { $gte: ["$activeSeconds", 10] }] }, 1, 0] } },
      firstObserved: { $max: "$firstObserved" }, platform: { $first: "$platform" }, versions: { $addToSet: "$version" },
    } },
    { $sort: { "_id.day": 1 } },
  ], { maxTimeMS: 20000 }).toArray();
  return summarizeNativeUsage(sessions, start, end);
}

export function summarizeNativeUsage(rows, start, end) {
  const apps = {}, installs = new Map();
  const empty = () => ({ activeInstalls: 0, opens: 0, firstObservedInstalls: 0, activeSeconds: 0,
    loadedSessions: 0, interactedSessions: 0, engagedSessions: 0 });
  for (const row of rows) {
    const { app, install, day } = row._id;
    const stats = apps[app] ??= { ...empty(), returningInstalls: 0, daily: {}, platforms: {} };
    const daily = stats.daily[day] ??= empty();
    const key = `${app}:${install}`;
    let seen = installs.get(key);
    if (!seen) { seen = { days: new Set(), first: false }; installs.set(key, seen); stats.activeInstalls++; }
    if (!seen.days.has(day)) {
      seen.days.add(day); daily.activeInstalls++;
      if (seen.days.size === 2) stats.returningInstalls++;
    }
    if (row.firstObserved) {
      daily.firstObservedInstalls++;
      if (!seen.first) { seen.first = true; stats.firstObservedInstalls++; }
    }
    for (const [target, source] of Object.entries({ opens: "opens", activeSeconds: "activeSeconds",
      loadedSessions: "loaded", interactedSessions: "interacted", engagedSessions: "engaged" })) {
      stats[target] += row[source] || 0; daily[target] += row[source] || 0;
    }
    stats.platforms[row.platform] = (stats.platforms[row.platform] || 0) + row.opens;
  }
  return { format: "ac.native-usage.v1", start, end, apps,
    note: "AC iOS 1.2+ only. Opens are foreground sessions, not background pushes or brief system interruptions. Active time is foreground time, not proof of attention; engaged requires a successful load, canvas interaction and 10 seconds. First observed includes upgrades, restores and reinstalls. Returning means activity on 2+ UTC days within this window. Offline snapshots can arrive 7 days late. Sessions belong to their start day. No account or device identity is collected." };
}

export async function nativeUsageDaily(db, start, end) {
  const report = await nativeUsageReport(db, start, end);
  return Object.fromEntries(Object.entries(report.apps).map(([app, { daily, platforms, returningInstalls, ...counts }]) => [app, counts]));
}
