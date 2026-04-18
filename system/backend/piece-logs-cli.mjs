// piece-logs-cli.mjs — admin CLI to inspect piece-run telemetry.
//
// Runs on lith (has MONGODB_CONNECTION_STRING in env via
// /opt/ac/system/.env). Reads the `piece-runs` collection populated
// by the /api/piece-log endpoint (see netlify/functions/piece-log.mjs
// and the console-capture wrapper near line 970 of
// system/public/aesthetic.computer/lib/disk.mjs).
//
// Intended invocation: via the ac-piece-logs fish function which
// SSHes to lith and runs `node system/backend/piece-logs-cli.mjs ...`
// with args. Works locally too if your shell has MONGODB_CONNECTION_STRING
// + MONGODB_NAME set (e.g. source /opt/ac/system/.env).
//
// Flags:
//   --slug <piece>     filter by slug (e.g. notepat)
//   --host <hostname>  filter by meta.host
//   --status <state>   started | complete | error
//   --since <min>      only runs updated in the last N minutes
//   --limit <n>        cap results (default 20, max 200)
//   --events           include the captured console events in each run
//   --errors-only      shorthand for --status error
//   --grep <pattern>   fetch --events then filter to runs whose events
//                      include `pattern` (case-insensitive regex)
//   --json             raw JSON (default is a human summary)
//
// Examples:
//   node system/backend/piece-logs-cli.mjs --slug notepat --limit 5 --events
//   node system/backend/piece-logs-cli.mjs --errors-only --since 60
//   node system/backend/piece-logs-cli.mjs --grep "drumMode" --limit 3

import { connect } from "./database.mjs";

const args = process.argv.slice(2);
const opts = {
  slug: null,
  host: null,
  status: null,
  since: null,
  limit: 20,
  events: false,
  grep: null,
  json: false,
};

for (let i = 0; i < args.length; i++) {
  const a = args[i];
  const take = () => args[++i];
  if (a === "--slug") opts.slug = take();
  else if (a === "--host") opts.host = take();
  else if (a === "--status") opts.status = take();
  else if (a === "--since") opts.since = Number(take());
  else if (a === "--limit") opts.limit = Math.min(200, Math.max(1, Number(take())));
  else if (a === "--events") opts.events = true;
  else if (a === "--errors-only") opts.status = "error";
  else if (a === "--grep") { opts.grep = take(); opts.events = true; }
  else if (a === "--json") opts.json = true;
  else if (a === "-h" || a === "--help") { printHelp(); process.exit(0); }
  else {
    console.error(`Unknown flag: ${a}`);
    printHelp();
    process.exit(2);
  }
}

function printHelp() {
  console.log(`piece-logs-cli — inspect piece-run telemetry

Usage: node system/backend/piece-logs-cli.mjs [flags]

Flags:
  --slug <piece>     filter by slug (e.g. notepat)
  --host <hostname>  filter by meta.host
  --status <state>   started | complete | error
  --since <min>      only runs updated in the last N minutes
  --limit <n>        cap results (default 20, max 200)
  --events           include captured console events
  --errors-only      shorthand for --status error
  --grep <regex>     filter to runs whose events match regex (implies --events)
  --json             raw JSON output
`);
}

const query = {};
if (opts.slug) query["meta.slug"] = opts.slug;
if (opts.host) query["meta.host"] = opts.host;
if (opts.status) query.status = opts.status;
if (opts.since) query.updatedAt = { $gte: new Date(Date.now() - opts.since * 60_000) };

const projection = { _id: 0 };
if (!opts.events) projection.events = 0;

async function main() {
  const database = await connect();
  const runs = database.db.collection("piece-runs");
  let results = await runs.find(query, { projection }).sort({ updatedAt: -1 }).limit(opts.limit).toArray();
  await database.disconnect();

  if (opts.grep) {
    const re = new RegExp(opts.grep, "i");
    results = results.filter((run) => (run.events || []).some((ev) => re.test(ev.message || "")));
  }

  if (opts.json) {
    console.log(JSON.stringify(results, null, 2));
    return;
  }

  if (results.length === 0) {
    console.log("no matching runs");
    return;
  }

  for (const r of results) {
    const when = r.updatedAt ? new Date(r.updatedAt).toISOString() : "?";
    const status = r.status || "?";
    const slug = r.meta?.slug || r.slug || "?";
    const host = r.meta?.host || r.server?.country || "";
    console.log(`── ${when}  [${status.padEnd(8)}]  ${slug.padEnd(20)}  ${host}`);
    console.log(`   pieceId:  ${r.pieceId}`);
    if (r.meta?.userAgent) console.log(`   ua:       ${truncate(r.meta.userAgent, 100)}`);
    if (r.error) console.log(`   error:    ${r.error.message || JSON.stringify(r.error).slice(0, 200)}`);
    if (opts.events && r.events?.length) {
      console.log(`   events:   (${r.events.length})`);
      for (const ev of r.events.slice(-30)) {
        const elapsed = `${String(ev.elapsed ?? "?").padStart(6)}ms`;
        console.log(`     ${elapsed} ${ev.level.padEnd(5)} ${truncate(ev.message, 200)}`);
      }
    }
    console.log();
  }
}

function truncate(s, n) {
  s = String(s ?? "");
  return s.length > n ? s.slice(0, n - 1) + "…" : s;
}

main().catch((err) => {
  console.error("piece-logs-cli failed:", err?.stack || err);
  process.exit(1);
});
