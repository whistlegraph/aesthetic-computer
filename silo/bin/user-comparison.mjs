#!/usr/bin/env node
// silo/bin/user-comparison.mjs
//
// Generates a side-by-side HTML report comparing aesthetic.computer and
// sotce.net user funnels: Auth0 signups, claimed handles (split by the
// `sotce-` prefix in @handles._id), and active Stripe subscribers (sotce
// only). Output lands in aesthetic-computer-vault/reports/, which is
// gitignored from the public knot — keep the file out of the main repo.
//
// Usage: node silo/bin/user-comparison.mjs

import { writeFile, mkdir } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { config as loadEnv } from "dotenv";
import { MongoClient } from "mongodb";

const __dirname = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = resolve(__dirname, "../..");
const VAULT_ENV = resolve(REPO_ROOT, "aesthetic-computer-vault/lith/.env");
const VAULT_REPORTS = resolve(REPO_ROOT, "aesthetic-computer-vault/reports");

loadEnv({ path: VAULT_ENV });

const TENANTS = {
  aesthetic: {
    domain: "aesthetic.us.auth0.com",
    clientId: process.env.AUTH0_M2M_CLIENT_ID,
    clientSecret: process.env.AUTH0_M2M_SECRET,
  },
  sotce: {
    domain: "sotce.us.auth0.com",
    clientId: process.env.SOTCE_AUTH0_M2M_CLIENT_ID,
    clientSecret: process.env.SOTCE_AUTH0_M2M_SECRET,
  },
};

async function auth0Token(tenant) {
  const t = TENANTS[tenant];
  if (!t.clientId || !t.clientSecret) {
    throw new Error(`Missing Auth0 M2M credentials for ${tenant}`);
  }
  const res = await fetch(`https://${t.domain}/oauth/token`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      grant_type: "client_credentials",
      client_id: t.clientId,
      client_secret: t.clientSecret,
      audience: `https://${t.domain}/api/v2/`,
    }),
  });
  if (!res.ok) throw new Error(`Auth0 token (${tenant}): ${res.status}`);
  return (await res.json()).access_token;
}

// Auth0's /api/v2/users include_totals caps at 1000 (search-API limit).
// /api/v2/stats/daily returns per-day {signups, logins} buckets and is
// the only way to get an accurate all-time count without an export job.
async function auth0SignupTotal(tenant) {
  const token = await auth0Token(tenant);
  const url = new URL(`https://${TENANTS[tenant].domain}/api/v2/stats/daily`);
  url.searchParams.set("from", "20210101");
  const today = new Date();
  const end = `${today.getUTCFullYear()}${String(today.getUTCMonth() + 1).padStart(2, "0")}${String(today.getUTCDate()).padStart(2, "0")}`;
  url.searchParams.set("to", end);
  const res = await fetch(url, {
    headers: { authorization: `Bearer ${token}` },
  });
  if (!res.ok) throw new Error(`Auth0 stats/daily (${tenant}): ${res.status}`);
  const days = await res.json();
  if (!Array.isArray(days)) throw new Error(`Auth0 stats/daily (${tenant}): unexpected shape`);
  const signups = days.reduce((acc, d) => acc + (d.signups || 0), 0);
  const logins = days.reduce((acc, d) => acc + (d.logins || 0), 0);
  const firstDay = days[0]?.date || null;
  return { signups, logins, firstDay, dayCount: days.length };
}

async function handleCounts() {
  const uri = process.env.MONGODB_CONNECTION_STRING;
  if (!uri) throw new Error("Missing MONGODB_CONNECTION_STRING");
  const dbName = process.env.MONGODB_NAME || "aesthetic";
  const client = new MongoClient(uri);
  try {
    await client.connect();
    const handles = client.db(dbName).collection("@handles");
    const total = await handles.countDocuments();
    const sotce = await handles.countDocuments({ _id: { $regex: "^sotce-" } });
    return { total, sotce, aesthetic: total - sotce };
  } finally {
    await client.close();
  }
}

async function sotceSubscribers() {
  try {
    const res = await fetch("https://sotce.net/sotce-net/subscribers");
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const body = await res.json();
    return typeof body.subscribers === "number" ? body.subscribers : null;
  } catch (err) {
    return { error: String(err) };
  }
}

function pct(num, denom) {
  if (!denom || denom <= 0) return "—";
  return `${((num / denom) * 100).toFixed(1)}%`;
}

function fmt(n) {
  if (n === null || n === undefined) return "—";
  if (typeof n === "object") return "error";
  return n.toLocaleString("en-US");
}

function render({ data, generatedAt }) {
  const { aesthetic, sotce, handles, subscribers } = data;
  const subErr = subscribers && typeof subscribers === "object";
  const subValue = subErr ? null : subscribers;
  const acSignups = aesthetic?.signups ?? null;
  const sotceSignups = sotce?.signups ?? null;
  const acFirst = aesthetic?.firstDay?.slice(0, 10);
  const sotceFirst = sotce?.firstDay?.slice(0, 10);

  const rows = [
    {
      label: "Tenant first activity",
      ac: acFirst || "—",
      sotce: sotceFirst || "—",
      note: "First day with stats reported by Auth0 (proxy for tenant launch)",
      raw: true,
    },
    {
      label: "Auth0 signups (all-time)",
      ac: acSignups,
      sotce: sotceSignups,
      note: "Sum of /api/v2/stats/daily.signups (the /users endpoint caps total at 1k)",
    },
    {
      label: "Auth0 logins (all-time)",
      ac: aesthetic?.logins ?? null,
      sotce: sotce?.logins ?? null,
      note: "Lifetime authentications",
    },
    {
      label: "Active subscribers",
      ac: null,
      sotce: subValue,
      note: subErr
        ? `Stripe lookup failed: ${subscribers.error}`
        : "Sotce is paid; AC is free. Source: sotce.net/sotce-net/subscribers",
    },
    {
      label: "Handles claimed",
      ac: handles.aesthetic,
      sotce: handles.sotce,
      note: "MongoDB @handles split by 'sotce-' _id prefix",
    },
    {
      label: "Handle adoption",
      ac: pct(handles.aesthetic, acSignups),
      sotce: pct(handles.sotce, sotceSignups),
      note: "Handles ÷ signups",
      isPct: true,
    },
    {
      label: "Subscription rate",
      ac: "—",
      sotce: subErr ? "error" : pct(subValue, sotceSignups),
      note: "Subscribers ÷ signups",
      isPct: true,
    },
  ];

  const cell = (v, r) => {
    if (r.isPct) return v ?? "—";
    if (r.raw) return v ?? "—";
    return fmt(v);
  };

  const tableRows = rows
    .map(
      (r) => `
        <tr>
          <th scope="row">${r.label}</th>
          <td class="num">${cell(r.ac, r)}</td>
          <td class="num">${cell(r.sotce, r)}</td>
          <td class="note">${r.note}</td>
        </tr>`,
    )
    .join("");

  const totalSignups = (acSignups || 0) + (sotceSignups || 0);
  const totalHandles = handles.total;

  return `<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <title>user funnel — ac vs sotce — ${generatedAt}</title>
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <style>
    :root {
      --bg: #faf8f4;
      --ink: #1a1a1a;
      --muted: #6b6b6b;
      --rule: #d8d3c8;
      --ac: #2b6cb0;
      --sotce: #b03a8c;
    }
    @media (prefers-color-scheme: dark) {
      :root { --bg: #15140f; --ink: #f1ece0; --muted: #8a8377; --rule: #2c281f; --ac: #6cb0ff; --sotce: #ff86c8; }
    }
    html, body { background: var(--bg); color: var(--ink); }
    body { font: 16px/1.5 -apple-system, BlinkMacSystemFont, "SF Pro Text", system-ui, sans-serif; max-width: 980px; margin: 2rem auto; padding: 0 1.25rem; }
    h1 { font-size: 1.4rem; margin: 0 0 .25rem; letter-spacing: -0.01em; }
    .stamp { color: var(--muted); font-size: .9rem; margin-bottom: 1.75rem; }
    .totals { display: flex; gap: 1.25rem; flex-wrap: wrap; margin-bottom: 1.5rem; }
    .totals div { background: rgba(127,127,127,.06); border: 1px solid var(--rule); border-radius: .5rem; padding: .65rem .9rem; min-width: 9rem; }
    .totals strong { display: block; font-size: 1.55rem; letter-spacing: -0.02em; }
    .totals span { color: var(--muted); font-size: .82rem; }
    table { width: 100%; border-collapse: collapse; margin: .25rem 0 1rem; }
    th, td { padding: .65rem .75rem; border-bottom: 1px solid var(--rule); text-align: left; vertical-align: top; }
    thead th { font-weight: 600; color: var(--muted); font-size: .82rem; text-transform: uppercase; letter-spacing: .04em; }
    th[scope="row"] { font-weight: 500; width: 14rem; }
    .num { font-variant-numeric: tabular-nums; font-weight: 600; text-align: right; width: 8.5rem; }
    .note { color: var(--muted); font-size: .88rem; }
    .ac { color: var(--ac); }
    .sotce { color: var(--sotce); }
    footer { margin-top: 2rem; color: var(--muted); font-size: .82rem; }
    code { font: 13px/1.4 ui-monospace, "SF Mono", Menlo, monospace; background: rgba(127,127,127,.1); padding: .08em .35em; border-radius: .25rem; }
  </style>
</head>
<body>
  <h1>user funnel — aesthetic.computer vs sotce.net</h1>
  <div class="stamp">generated ${generatedAt}</div>

  <div class="totals">
    <div><strong>${fmt(totalSignups)}</strong><span>combined signups</span></div>
    <div><strong>${fmt(totalHandles)}</strong><span>combined handles</span></div>
    <div><strong>${fmt(subValue)}</strong><span>sotce subscribers</span></div>
  </div>

  <table>
    <thead>
      <tr>
        <th></th>
        <th class="num ac">aesthetic.computer</th>
        <th class="num sotce">sotce.net</th>
        <th>notes</th>
      </tr>
    </thead>
    <tbody>${tableRows}
    </tbody>
  </table>

  <footer>
    sources: <code>aesthetic.us.auth0.com</code> + <code>sotce.us.auth0.com</code> /api/v2/users · MongoDB <code>@handles</code> on silo · <code>sotce.net/sotce-net/subscribers</code> (Stripe-backed).
    handles split by the <code>sotce-</code> prefix on <code>_id</code> as written by <code>system/netlify/functions/handle.mjs</code>.
    re-run with <code>node silo/bin/user-comparison.mjs</code>.
  </footer>
</body>
</html>
`;
}

async function main() {
  const [aesthetic, sotce, handles, subscribers] = await Promise.all([
    auth0SignupTotal("aesthetic").catch((e) => ({ error: String(e) })),
    auth0SignupTotal("sotce").catch((e) => ({ error: String(e) })),
    handleCounts(),
    sotceSubscribers(),
  ]);

  const data = { aesthetic, sotce, handles, subscribers };
  const generatedAt = new Date().toISOString().replace("T", " ").slice(0, 19) + " UTC";
  const html = render({ data, generatedAt });

  await mkdir(VAULT_REPORTS, { recursive: true });
  const date = new Date().toISOString().slice(0, 10);
  const out = resolve(VAULT_REPORTS, `${date}-user-comparison.html`);
  await writeFile(out, html, "utf8");

  console.log(JSON.stringify(data, null, 2));
  console.log(`\n→ ${out}`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
