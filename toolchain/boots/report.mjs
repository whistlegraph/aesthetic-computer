// The Last Week of Boots — aggregate the `boots` telemetry collection.
//
// Reads only. Answers the questions nobody has been asking of this data: how
// many boots fail, what they fail with, which pieces they fail on, and whether
// any of it is getting worse.
//
//   node toolchain/boots/report.mjs                # last 7 days, markdown
//   node toolchain/boots/report.mjs --days 365     # this year so far
//   node toolchain/boots/report.mjs --json         # machine-readable (menu bar)
//   node toolchain/boots/report.mjs --include-dev  # keep localhost boots in
//
// Needs MONGODB_CONNECTION_STRING (and optionally MONGODB_NAME) in the env.

import { MongoClient } from "mongodb";

const args = process.argv.slice(2);
const flag = (name) => args.includes(`--${name}`);
const opt = (name, fallback) => {
  const i = args.indexOf(`--${name}`);
  return i !== -1 && args[i + 1] ? args[i + 1] : fallback;
};

const DAYS = Number(opt("days", 7));
const AS_JSON = flag("json");
const INCLUDE_DEV = flag("include-dev");
const TOP = Number(opt("top", 12));

const uri = process.env.MONGODB_CONNECTION_STRING;
const dbName = process.env.MONGODB_NAME;
// Both are required. Guessing the database name would query an empty db and
// print a confident report of all zeros, which is worse than failing.
if (!uri || !dbName) {
  console.error(
    `Missing ${!uri ? "MONGODB_CONNECTION_STRING" : ""}${!uri && !dbName ? " and " : ""}${!dbName ? "MONGODB_NAME" : ""}.\n` +
      "This script only reads. Run it where the env is already loaded.",
  );
  process.exit(1);
}

const since = new Date(Date.now() - DAYS * 24 * 60 * 60 * 1000);

// Real traffic only, unless asked otherwise. localhost boots are @jeffrey
// hitting reload, and they would swamp every rate in here.
const scope = { createdAt: { $gte: since } };
if (!INCLUDE_DEV) scope["meta.localDev"] = { $ne: true };

const client = new MongoClient(uri);
await client.connect();
const boots = client.db(dbName).collection("boots");

const count = (q) => boots.countDocuments({ ...scope, ...q });

// A boot that started and never reported success or error is an abandoned boot:
// the user left, or something wedged hard enough that even the error handler
// never ran. Those are interesting precisely because they are invisible today.
const [total, ok, failed, stuck] = await Promise.all([
  count({}),
  count({ status: "success" }),
  count({ status: "error" }),
  count({ status: "started" }),
]);

const top = async (field, extra = {}) =>
  boots
    .aggregate([
      { $match: { ...scope, ...extra } },
      { $group: { _id: `$${field}`, n: { $sum: 1 } } },
      { $sort: { n: -1 } },
      { $limit: TOP },
    ])
    .toArray();

// Daily rate, so a regression shows up as a shape rather than a single number.
const daily = await boots
  .aggregate([
    { $match: scope },
    {
      $group: {
        _id: { $dateToString: { format: "%Y-%m-%d", date: "$createdAt" } },
        n: { $sum: 1 },
        errors: { $sum: { $cond: [{ $eq: ["$status", "error"] }, 1, 0] } },
        stuck: { $sum: { $cond: [{ $eq: ["$status", "started"] }, 1, 0] } },
      },
    },
    { $sort: { _id: 1 } },
  ])
  .toArray();

// Two places an error can hide: the `error` field set by phase=error, and any
// event logged at level=error during an otherwise "successful" boot. The second
// kind is completely invisible in the status counts.
const errorDocs = await boots
  .aggregate([
    { $match: { ...scope, status: "error" } },
    { $project: { msg: { $ifNull: ["$error.error.message", "$error.error"] }, path: "$meta.path", browser: "$meta.browser" } },
    { $limit: 5000 },
  ])
  .toArray();

const loggedErrorEvents = await boots
  .aggregate([
    { $match: { ...scope, "events.level": "error" } },
    { $unwind: "$events" },
    { $match: { "events.level": "error" } },
    { $group: { _id: "$events.message", n: { $sum: 1 } } },
    { $sort: { n: -1 } },
    { $limit: TOP },
  ])
  .toArray();

const normalize = (m) => {
  const s = typeof m === "string" ? m : JSON.stringify(m ?? "unknown");
  return s
    .replace(/https?:\/\/[^\s"')]+/g, "<url>")
    .replace(/\b[0-9a-f]{8,}\b/gi, "<hash>")
    .replace(/\d+/g, "N")
    .slice(0, 160);
};

const errorsByMessage = {};
for (const d of errorDocs) {
  const k = normalize(d.msg);
  (errorsByMessage[k] ??= { n: 0, paths: new Set(), browsers: new Set() });
  errorsByMessage[k].n++;
  if (d.path) errorsByMessage[k].paths.add(d.path);
  if (d.browser) errorsByMessage[k].browsers.add(d.browser);
}
const topErrors = Object.entries(errorsByMessage)
  .sort((a, b) => b[1].n - a[1].n)
  .slice(0, TOP)
  .map(([msg, v]) => ({
    msg,
    n: v.n,
    paths: [...v.paths].slice(0, 4),
    browsers: [...v.browsers].slice(0, 4),
  }));

const [byPath, byBrowser, byCountry, byReferrer, failingPaths] = await Promise.all([
  top("meta.path"),
  top("meta.browser"),
  top("server.country"),
  top("meta.referrer"),
  top("meta.path", { status: "error" }),
]);

const mobile = await count({ "meta.mobile": true });
const embedded = await count({ "meta.embedded": true });
const signedIn = await count({ "meta.user.sub": { $exists: true, $ne: null } });

await client.close();

const pct = (n) => (total ? ((n / total) * 100).toFixed(1) + "%" : "—");

const report = {
  window: { days: DAYS, since: since.toISOString(), includeDev: INCLUDE_DEV },
  totals: { total, ok, failed, stuck },
  rates: { success: pct(ok), error: pct(failed), abandoned: pct(stuck) },
  daily,
  topErrors,
  loggedErrorEvents,
  failingPaths,
  byPath,
  byBrowser,
  byCountry,
  byReferrer,
  audience: { mobile, embedded, signedIn },
};

if (AS_JSON) {
  console.log(JSON.stringify(report, null, 2));
  process.exit(0);
}

const rows = (list, label) =>
  list
    .filter((r) => r._id)
    .map((r) => `| ${String(r._id).slice(0, 60)} | ${r.n} |`)
    .join("\n") || `| (no ${label}) | |`;

console.log(`# The Last ${DAYS === 7 ? "Week" : DAYS + " Days"} of Boots

Window: ${since.toISOString().slice(0, 10)} → today${INCLUDE_DEV ? "" : " · localhost boots excluded"}

## The headline

| | count | share |
|---|---|---|
| boots | ${total} | |
| succeeded | ${ok} | ${pct(ok)} |
| **errored** | **${failed}** | **${pct(failed)}** |
| never finished | ${stuck} | ${pct(stuck)} |

"Never finished" means a boot reported \`start\` and then nothing — the user left, or
it wedged before the error handler could fire. These are invisible today.

## Boots per day

| day | boots | errors | never finished |
|---|---|---|---|
${daily.map((d) => `| ${d._id} | ${d.n} | ${d.errors} | ${d.stuck} |`).join("\n")}

## What is actually breaking

| error | hits | seen on |
|---|---|---|
${topErrors.map((e) => `| \`${e.msg}\` | ${e.n} | ${e.paths.join(", ") || "—"} |`).join("\n") || "| (none) | | |"}

### Errors logged *during otherwise successful boots*

These never set \`status: "error"\`, so no success-rate number will ever show them.

| message | hits |
|---|---|
${loggedErrorEvents.map((e) => `| \`${normalize(e._id)}\` | ${e.n} |`).join("\n") || "| (none) | |"}

## Pieces that fail most

| path | errored boots |
|---|---|
${rows(failingPaths, "failing paths")}

## Most-booted pieces

| path | boots |
|---|---|
${rows(byPath, "paths")}

## Who is booting

- mobile: ${mobile} (${pct(mobile)})
- embedded in an iframe: ${embedded} (${pct(embedded)})
- signed in: ${signedIn} (${pct(signedIn)})

| browser | boots |
|---|---|
${rows(byBrowser, "browsers")}

| country | boots |
|---|---|
${rows(byCountry, "countries")}

| referrer | boots |
|---|---|
${rows(byReferrer, "referrers")}
`);
