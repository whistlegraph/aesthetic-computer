#!/usr/bin/env node
// Prompt Stats, 2026.09.03
// Search and tally the prompt corpus recorded by /api/prompt-log.
// Auth: admin token from ~/.ac-token (shared session).
//
//   node toolchain/prompt-stats.mjs                     top entries, last 7 days
//   node toolchain/prompt-stats.mjs --since 2026-09-01  top entries since a date
//   node toolchain/prompt-stats.mjs --misses            top unresolved entries
//   node toolchain/prompt-stats.mjs --q flower          search raw entries
//   node toolchain/prompt-stats.mjs --kind kidlisp-code --limit 100

import { readFileSync } from "fs";
import { homedir } from "os";
import { join } from "path";

const HOST = process.env.AC_HOST || "https://aesthetic.computer";

const args = process.argv.slice(2);
function flag(name) {
  const i = args.indexOf(`--${name}`);
  if (i === -1) return null;
  const next = args[i + 1];
  return next && !next.startsWith("--") ? next : true;
}

let token;
try {
  token = JSON.parse(readFileSync(join(homedir(), ".ac-token"), "utf8"))
    .access_token;
} catch {
  console.error("❌ No admin token — expected ~/.ac-token with access_token.");
  process.exit(1);
}

const q = flag("q");
const kind = flag("kind");
const limit = flag("limit") || 50;
const since =
  flag("since") ||
  new Date(Date.now() - 7 * 24 * 3600 * 1000).toISOString().slice(0, 10);

const params = new URLSearchParams({ limit });
let mode;
if (q) {
  mode = "search";
  params.set("q", q);
  if (kind) params.set("kind", kind);
} else {
  mode = "stats";
  params.set("stats", "1");
  params.set("since", since);
  if (kind) params.set("kind", kind);
  if (flag("misses")) params.set("resolved", "false");
}

const res = await fetch(`${HOST}/api/prompt-log?${params}`, {
  headers: { Authorization: `Bearer ${token}` },
});
if (!res.ok) {
  console.error(`❌ ${res.status} ${await res.text()}`);
  process.exit(1);
}
const data = await res.json();

if (mode === "search") {
  for (const e of data.entries) {
    const mark = e.resolved === false ? "✗" : e.resolved ? "·" : "?";
    console.log(
      `${e.createdAt.slice(0, 16)} ${mark} [${e.kind}] ${e.text}`,
    );
  }
  console.log(`\n${data.entries.length} entries`);
} else {
  console.log(`since ${since}${kind ? ` · kind=${kind}` : ""}\n`);
  for (const t of data.top) {
    const mark = t.resolved === false ? "✗" : t.resolved ? "·" : "?";
    console.log(`${String(t.count).padStart(6)}  ${mark} [${t.kind}] ${t._id}`);
  }
  console.log("\ntotals by kind:");
  for (const t of data.totals) {
    console.log(`${String(t.count).padStart(6)}  ${t._id}`);
  }
}
