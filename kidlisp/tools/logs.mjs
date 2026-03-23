#!/usr/bin/env node
// KidLisp Log Viewer (admin-only)
// Pulls client-side telemetry from /api/kidlisp-log and /api/boot-log
//
// Auth: set AC_ADMIN_TOKEN env var or pass --token <bearer-token>
//
// Usage:
//   ./logs.mjs                     # recent kidlisp logs (default 20)
//   ./logs.mjs --limit 50          # last 50 entries
//   ./logs.mjs --type gpu-disabled # filter by log type
//   ./logs.mjs --since 2026-03-20  # logs since date
//   ./logs.mjs --boots             # boot telemetry instead
//   ./logs.mjs --boots --errors    # only boot errors
//   ./logs.mjs --stats             # type breakdown counts
//   ./logs.mjs --token <token>     # pass auth token inline

const BASE = process.env.AC_API_URL || "https://aesthetic.computer";

const args = process.argv.slice(2);
function flag(name) {
  return args.includes(`--${name}`);
}
function opt(name, fallback) {
  const i = args.indexOf(`--${name}`);
  return i !== -1 && args[i + 1] ? args[i + 1] : fallback;
}

const token = opt("token", null) || process.env.AC_ADMIN_TOKEN;
if (!token) {
  console.error(
    "Admin token required. Set AC_ADMIN_TOKEN or pass --token <bearer-token>",
  );
  process.exit(1);
}

const authHeaders = { Authorization: `Bearer ${token}` };

async function main() {
  if (flag("boots")) {
    await fetchBoots();
  } else {
    await fetchKidlispLogs();
  }
}

async function fetchKidlispLogs() {
  const limit = opt("limit", "20");
  const type = opt("type", null);
  const since = opt("since", null);
  const ua = opt("ua", null);

  const params = new URLSearchParams({ limit });
  if (type) params.set("type", type);
  if (since) params.set("since", since);
  if (ua) params.set("ua", ua);

  const url = `${BASE}/api/kidlisp-log?${params}`;
  const res = await fetch(url, { headers: authHeaders });
  if (!res.ok) {
    console.error(`Error ${res.status}: ${await res.text()}`);
    process.exit(1);
  }

  const { logs, stats } = await res.json();

  if (flag("stats")) {
    console.log("\n  Type breakdown:");
    for (const s of stats) {
      console.log(`    ${s._id}: ${s.count}`);
    }
    console.log();
    return;
  }

  if (logs.length === 0) {
    console.log("No logs found.");
    return;
  }

  for (const log of logs) {
    const date = new Date(log.createdAt).toLocaleString();
    const device = log.device?.userAgent?.slice(0, 60) || "unknown";
    const country = log.server?.country || "??";
    const detail = log.detail
      ? typeof log.detail === "object"
        ? JSON.stringify(log.detail)
        : log.detail
      : "";
    console.log(
      `[${date}] ${log.type || "?"}  ${country}  ${log.effect || ""}  ${detail}`,
    );
    console.log(`  device: ${device}`);
    if (log.gpuStatus) {
      const failed = Object.entries(log.gpuStatus)
        .filter(([, v]) => v > 0)
        .map(([k, v]) => `${k}:${v}`);
      if (failed.length) console.log(`  gpu failures: ${failed.join(", ")}`);
    }
  }
  console.log(`\n  Showing ${logs.length} entries.`);
}

async function fetchBoots() {
  const limit = opt("limit", "30");
  const params = new URLSearchParams({ limit });

  const url = `${BASE}/api/boot-log?${params}`;
  const res = await fetch(url, { headers: authHeaders });
  if (!res.ok) {
    console.error(`Error ${res.status}: ${await res.text()}`);
    process.exit(1);
  }

  const { boots } = await res.json();

  const errorsOnly = flag("errors");
  const filtered = errorsOnly
    ? boots.filter((b) => b.status === "error")
    : boots;

  if (filtered.length === 0) {
    console.log(errorsOnly ? "No boot errors found." : "No boot logs found.");
    return;
  }

  for (const boot of filtered) {
    const date = new Date(boot.createdAt).toLocaleString();
    const status = boot.status || "?";
    const host = boot.meta?.host || "?";
    const path = boot.meta?.path || "/";
    const country = boot.server?.country || "??";

    const icon =
      status === "error" ? "X" : status === "success" ? "+" : "-";

    console.log(`[${icon}] ${date}  ${status}  ${country}  ${host}${path}`);

    if (boot.error) {
      const errMsg =
        typeof boot.error === "string"
          ? boot.error
          : boot.error.message || JSON.stringify(boot.error).slice(0, 120);
      console.log(`    error: ${errMsg}`);
    }

    if (boot.events?.length) {
      const errEvents = boot.events.filter(
        (e) => e.level === "error" || e.label?.includes("error"),
      );
      for (const ev of errEvents) {
        console.log(`    event: ${ev.label || ev.message || JSON.stringify(ev)}`);
      }
    }
  }
  console.log(`\n  Showing ${filtered.length} of ${boots.length} entries.`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
