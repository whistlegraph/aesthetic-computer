#!/usr/bin/env node
// pulse.mjs — the Aesthetic network on one card: the last 24 hours plus a daily chart.
//
//   npm run pulse                 # 24h card + 14-day chart
//   npm run pulse -- --days 30    # longer chart (max 35)
//
// Reads the same numbers as the analytics MCP: it asks the shared daemon on
// :7788 when it is up and otherwise spawns toolchain/mcp/analytics-mcp.mjs over
// stdio. Sign-ups come from signup-report.mjs on lith over ssh. Read-only.
// Colour only on a TTY (and never with NO_COLOR), so the card pastes cleanly.

import { execFile, spawn } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

const ROOT = join(dirname(fileURLToPath(import.meta.url)), "../..");
const DAEMON = "http://127.0.0.1:7788/mcp/analytics";
const LITH = "root@lith.aesthetic.computer";
const SSH_KEY = join(ROOT, "aesthetic-computer-vault/home/.ssh/id_rsa");
const pexec = promisify(execFile);

const args = process.argv.slice(2);
let days = 14;
for (let i = 0; i < args.length; i++) {
  if (args[i] === "--days") days = Number(args[++i]);
  else throw new Error("Usage: pulse.mjs [--days 2..35]");
}
if (!Number.isInteger(days) || days < 2 || days > 35) throw new Error("--days must be 2..35");

// 🎨 Ink

const color = process.stdout.isTTY && !process.env.NO_COLOR;
const ink = (code) => (s) => color ? `\x1b[${code}m${s}\x1b[0m` : String(s);
const dim = ink("2"), bold = ink("1"), cyan = ink("36"), pink = ink("35"), green = ink("32"), yellow = ink("33");

const BLOCKS = " ▏▎▍▌▋▊▉█";
const SPARK = "▁▂▃▄▅▆▇█";
const bar = (n, max, width) => {
  const eighths = max ? Math.round((n / max) * width * 8) : 0;
  const full = Math.floor(eighths / 8), part = eighths % 8;
  return ("█".repeat(full) + (part ? BLOCKS[part] : "") || (n ? "▏" : "")).padEnd(width);
};
const spark = (values) => {
  const max = Math.max(...values);
  return values.map((v) => v == null ? " " : !max || !v ? dim("·") : SPARK[Math.min(7, Math.floor((v / max) * 7.999))]).join("");
};
const num = (n) => Number(n || 0).toLocaleString("en-US");
const pad = (s, w) => String(s).padStart(w);

// 🔌 Data

let stdioServer;
async function tool(name, toolArgs = {}) {
  const message = { jsonrpc: "2.0", id: Math.random(), method: "tools/call", params: { name, arguments: toolArgs } };
  let response;
  try {
    const res = await fetch(DAEMON, { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify(message) });
    response = await res.json();
  } catch {
    response = await viaStdio(message);
  }
  const result = response.result;
  if (!result || result.isError) throw new Error(`${name}: ${result?.content?.[0]?.text || response.error?.message}`);
  return JSON.parse(result.content[0].text);
}

// No daemon: one stdio child serves every call, matched by id.
function viaStdio(message) {
  if (!stdioServer) {
    const child = spawn("node", [join(ROOT, "toolchain/mcp/analytics-mcp.mjs")], { stdio: ["pipe", "pipe", "ignore"] });
    const waiting = new Map();
    let buffer = "";
    child.stdout.on("data", (chunk) => {
      buffer += chunk;
      let newline;
      while ((newline = buffer.indexOf("\n")) >= 0) {
        const line = buffer.slice(0, newline); buffer = buffer.slice(newline + 1);
        try { const reply = JSON.parse(line); waiting.get(reply.id)?.(reply); waiting.delete(reply.id); } catch {}
      }
    });
    stdioServer = { child, waiting };
  }
  return new Promise((resolve) => {
    stdioServer.waiting.set(message.id, resolve);
    stdioServer.child.stdin.write(JSON.stringify(message) + "\n");
  });
}

async function signups(reportDays) {
  const remote = `cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/signup-report.mjs --days ${reportDays}`;
  const { stdout } = await pexec("ssh", ["-i", SSH_KEY, "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", LITH, remote],
    { timeout: 90_000, maxBuffer: 8 * 1024 * 1024 });
  return JSON.parse(stdout.slice(stdout.indexOf("{"))); // database.mjs logs a line first
}

const soft = (promise) => promise.catch((error) => ({ error: error.message }));
const [visits, daily, refs, activity, downloads, opens, signupDays, signupToday] = await Promise.all([
  tool("visits_report", { hours: 24 }),
  tool("daily_metrics", { days, appStore: true }),
  soft(tool("network_referrers", { hours: 24, limit: 40 })),
  soft(tool("account_activity", { hours: 24, limit: 500 })),
  soft(tool("direct_downloads", { days: 1 })),
  soft(tool("app_opens", { days: 1 })),
  soft(signups(days)),
  soft(signups(1)),
]);
stdioServer?.child.kill();

// 📅 Days, oldest first, ending yesterday (the rollup only holds finished UTC days).

const dayKeys = [];
for (let i = days; i >= 1; i--) dayKeys.push(new Date(Date.now() - i * 86400000).toISOString().slice(0, 10));
const dayRow = (key) => daily.days[key] || {};
const siteVisits = (key) => Object.values(dayRow(key).visits || {}).reduce((n, s) => n + (s.visits || 0), 0);
const siteEngaged = (key) => Object.values(dayRow(key).visits || {}).reduce((n, s) => n + (s.engaged || 0), 0);
const perSite = (site) => dayKeys.map((k) => dayRow(k).visits?.[site]?.visits || 0);
const appOpens = (key) => Object.values(dayRow(key).opens || {}).reduce((n, a) => n + (a.opens || 0), 0);
const dmgs = (key) => Object.values(dayRow(key).downloads || {}).reduce((n, a) => n + (a.downloads || 0), 0);
const storeFirst = (key) => Object.values(dayRow(key).appStore || {})
  .reduce((n, kinds) => n + Object.entries(kinds).filter(([k]) => k.startsWith("First-time")).reduce((m, [, v]) => m + v, 0), 0);
const signupsOn = (key) => signupDays.daily?.find((d) => d.end.slice(0, 10) === key)?.accountsCreated ?? 0;

// 🖨 Card

const out = [];
const line = (s = "") => out.push(`  ${dim("│")}  ${s}`);
const label = (s) => bold(s.padEnd(6));
const now = new Date();
const stamp = `${now.toISOString().slice(5, 10).replace("-", "/")} ${now.toISOString().slice(11, 16)} utc`;
out.push("", `  ${dim("╭─")} ${bold("aesthetic network")} ${dim("· 24h ─────────────────────────────")} ${dim(stamp)} ${dim("─")}`);
line();

// WEB
const t = visits.totals;
line(`${label("WEB")}  ${bold(num(t.visits))} visits   ${green(num(t.engaged))} engaged   ${dim(`${num(t.automated)} bots dropped`)}`);
line(`        ${dim(`${"site".padEnd(19)} ${"24h".padStart(28)}   ${days}d trend`)}`);
const sites = Object.entries(visits.sites);
const top = sites.slice(0, 5), rest = sites.slice(5);
const maxSite = top[0]?.[1].visits || 0;
for (const [site, s] of top) {
  const extra = [s.actions.painting_edited && `${s.actions.painting_edited} edits`, s.actions.painting_saved && `${s.actions.painting_saved} saves`,
    s.actions.media_started && `${s.actions.media_started} plays`].filter(Boolean).join(" · ");
  line(`  ${site.padEnd(19)} ${cyan(bar(s.visits, maxSite, 24))} ${pad(s.visits, 4)}   ${spark(perSite(site))}${extra ? `  ${dim(extra)}` : ""}`);
}
if (rest.length) {
  const n = rest.reduce((m, [, s]) => m + s.visits, 0);
  line(`  ${`${rest.length} others`.padEnd(19)} ${cyan(bar(n, maxSite, 24))} ${pad(n, 4)}   ${dim(rest.map(([s]) => s).join(" "))}`);
}
line();

// DAILY — a column chart of human visits, engaged visits in brighter ink.
const totals = dayKeys.map(siteVisits), engaged = dayKeys.map(siteEngaged);
const peak = Math.max(...totals, 1), rows = 7;
line(`${label("DAILY")}  human visits per UTC day ${dim(`· peak ${num(peak)} · ${color ? "bright = engaged" : "█ visits"}`)}`);
for (let r = rows; r >= 1; r--) {
  const cells = totals.map((v, i) => {
    const h = (v / peak) * rows, e = (engaged[i] / peak) * rows;
    const glyph = h >= r ? "█" : h > r - 1 ? SPARK[Math.max(0, Math.floor((h - (r - 1)) * 8) - 1)] : " ";
    return (e >= r - 0.5 ? green : cyan)(glyph.repeat(2));
  });
  const axis = r === rows ? pad(num(peak), 5) : r === 1 ? pad(0, 5) : "     ";
  line(`${dim(axis)} ${dim("┤")}${cells.join(" ")}`);
}
line(`      ${dim("└" + dayKeys.map(() => "──").join("─"))}`);
line(`       ${dim(dayKeys.map((k, i) => i % 2 === (days - 1) % 2 ? k.slice(8) : "  ").join(" "))}`);
line();

// LIVE — signed-in accounts and what they opened.
if (!activity.error) {
  const byAccount = {}, byPiece = {};
  for (const e of activity.events) {
    byAccount[e.account] = (byAccount[e.account] || 0) + 1;
    if (e.action === "piece_opened") byPiece[e.piece] = (byPiece[e.piece] || 0) + 1;
  }
  const ranked = (o, n) => Object.entries(o).sort((a, b) => b[1] - a[1]).slice(0, n);
  line(`${label("LIVE")}  ${bold(activity.totals.accounts)} signed-in accounts · ${num(activity.totals.events)} events`);
  line(`        ${dim("loudest")} ${ranked(byAccount, 6).map(([a]) => a).join(" ")}`);
  line(`        ${dim("pieces ")} ${ranked(byPiece, 6).map(([p, n]) => `${p} ${dim(n)}`).join("  ")}`);
  if (activity.truncated) line(`        ${dim(`(accounts & pieces from the latest ${activity.events.length} events)`)}`);
  line();
}

// SIGNUP — accounts made, and how far today's attempts got.
if (!signupToday.error) {
  const f = { signup: { att: 0, auth: 0, done: 0 }, login: { att: 0, auth: 0, done: 0 } };
  for (const row of signupToday.funnel?.rows || []) {
    const m = f[row._id.mode]; if (!m) continue;
    m.att += row.attempts; m.auth += row.auth_returned; m.done += row.completed;
  }
  const made = signupToday.last24Hours?.accountsCreated ?? 0;
  const latest = signupDays.latestHandle;
  line(`${label("SIGNUP")}  ${bold(made)} new accounts   ${spark(dayKeys.map(signupsOn))} ${dim(`${days}d`)}${latest ? `   ${dim(`latest @${latest.value} ${latest.when.slice(5, 10)}`)}` : ""}`);
  const flow = (m) => `${m.att} tried → ${m.auth} back from auth → ${m.done} done`;
  line(`        ${dim("sign up")} ${f.signup.att && !f.signup.done ? yellow(flow(f.signup)) : flow(f.signup)}`);
  line(`        ${dim("log in ")} ${flow(f.login)}`);
  const give = visits.sites["give.aesthetic.computer"]?.visits || 0;
  const support = sites.reduce((n, [, s]) => n + (s.surfaces.support || 0), 0);
  line(`        ${dim("support")} give.aesthetic.computer ${give} visits · support pages ${support}`);
  line();
}

// APPS
const appLines = [];
if (!downloads.error) for (const [app, d] of Object.entries(downloads.apps)) {
  const plats = Object.entries(d.platforms).map(([p, n]) => `${n} ${p}`).join(" · ");
  appLines.push(`${app.padEnd(9)} ${bold(d.downloads)} dmg ${dim(`(${plats || "—"})`)}`);
}
if (!opens.error) {
  const today = now.toISOString().slice(0, 10);
  const opened = Object.entries(opens.apps).map(([app, a]) => [app, a.daily[today]?.opens || 0]).filter(([, n]) => n);
  if (opened.length) appLines.push(`opens     ${opened.map(([a, n]) => `${a} ${n}`).join(" · ")} ${dim("(today so far)")}`);
}
line(`${label("APPS")}  ${appLines[0] || dim("no direct downloads")}`);
for (const l of appLines.slice(1)) line(`        ${l}`);
line(`        ${dim("opens  ")} ${spark(dayKeys.map(appOpens))}  ${dim("dmg")} ${spark(dayKeys.map(dmgs))}  ${dim("app store new")} ${spark(dayKeys.map(storeFirst))}`);
line();

// REFS
if (!refs.error) {
  const byHost = {};
  for (const r of refs.visits) {
    const host = (r.referrerHost || "direct").replace(/^(www|m|l)\./, "");
    if (visits.sites[host] || host.endsWith("aesthetic.computer") || host.endsWith("sotce.net") || host === "sotce.com") continue;
    byHost[host] = (byHost[host] || 0) + r.visits;
  }
  const ranked = Object.entries(byHost).sort((a, b) => b[1] - a[1]).slice(0, 7);
  line(`${label("REFS")}  ${ranked.map(([h, n]) => `${h === "direct" ? dim(h) : pink(h)} ${n}`).join(dim(" · "))}`);
}
const failed = [refs, activity, downloads, opens, signupToday].filter((r) => r.error).map((r) => r.error.split(":")[0]);
if (failed.length) line(dim(`(unavailable: ${failed.join(", ")})`));
out.push(`  ${dim("╰─")}`, "");
console.log(out.join("\n"));
