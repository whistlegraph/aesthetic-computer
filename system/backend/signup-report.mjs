// Auth0 account records are the authority; webhook completeness is diagnostic.
// Read-only. No email, IP, token or submitted handle is returned in this report.
export async function readSignupAccounts({ start, end, fetch: request = fetch, env = process.env }) {
  const base = "https://aesthetic.us.auth0.com";
  async function json(url, options = {}) {
    const response = await request(url, { ...options, signal: AbortSignal.timeout(15000) });
    if (!response.ok) throw new Error(`Auth0 ${new URL(url).pathname}: HTTP ${response.status}`);
    return response.json();
  }
  const token = await json(`${base}/oauth/token`, {
    method: "POST", headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ client_id: env.AUTH0_M2M_CLIENT_ID, client_secret: env.AUTH0_M2M_SECRET,
      audience: `${base}/api/v2/`, grant_type: "client_credentials" }),
  });
  const headers = { Authorization: `Bearer ${token.access_token}` };
  async function window(from, until) {
    const rows = [];
    for (let page = 0; page < 10; page++) {
      const url = new URL(`${base}/api/v2/users`);
      url.search = new URLSearchParams({ q: `created_at:[${from.toISOString()} TO ${until.toISOString()}]`,
        search_engine: "v3", sort: "created_at:1", include_totals: "true", per_page: "100", page: String(page),
        fields: "user_id,created_at,email_verified", include_fields: "true" });
      const result = await json(url, { headers });
      // Auth0 caps search at 1000. Split the time window rather than silently
      // calling that cap a count. Deduplicate inclusive split boundaries later.
      if (page === 0 && result.total >= 1000) {
        if (+until - +from <= 1000) throw new Error("Signup interval exceeds Auth0 search capacity");
        const middle = new Date(Math.floor((+from + +until) / 2));
        return [...await window(from, middle), ...await window(middle, until)];
      }
      if (!Array.isArray(result.users) || !Number.isFinite(result.total)) throw new Error("Invalid Auth0 signup response");
      rows.push(...result.users);
      if (rows.length >= result.total) return rows;
      if (result.users.length === 0) throw new Error("Incomplete Auth0 signup response");
    }
    throw new Error("Incomplete Auth0 signup search");
  }
  const users = await window(start, end);
  return [...new Map(users.filter(row => new Date(row.created_at) >= start && new Date(row.created_at) < end)
    .map(row => [row.user_id, row])).values()];
}

export function summarizeSignupCohort(accounts, handleOwners, handleEvents, start, end) {
  const cohort = accounts.filter(row => new Date(row.created_at) >= start && new Date(row.created_at) < end);
  const completed = cohort.filter(row => handleOwners.has(row.user_id)).length;
  return { start, end, accountsCreated: cohort.length,
    emailVerifiedNow: cohort.filter(row => row.email_verified === true).length,
    cohortWithHandleNow: completed,
    accountToHandlePercent: cohort.length ? Math.round(completed / cohort.length * 1000) / 10 : null,
    // Existing accounts can claim a first handle; this is deliberately separate.
    handlesCreated: handleEvents.filter(row => row.when >= start && row.when < end).length };
}

export async function signupReport({ db, accounts, start, end }) {
  const ids = accounts.map(row => row.user_id);
  const owners = await db.collection("@handles").find({ _id: { $in: ids } }, { projection: { _id: 1 } }).toArray();
  const handles = await db.collection("logs").find({ action: "handle:create", when: { $gte: start, $lt: end } },
    { projection: { _id: 0, when: 1 } }).toArray();
  const ownerSet = new Set(owners.map(row => row._id));
  const summary = (from, until) => summarizeSignupCohort(accounts, ownerSet, handles, from, until);
  const daily = [];
  for (let from = +start; from < +end; from += 86400000) daily.push(summary(new Date(from), new Date(Math.min(from + 86400000, +end))));
  const latestHandle = await db.collection("logs").find({ action: "handle:create" },
    { projection: { _id: 0, value: 1, when: 1 } }).sort({ when: -1 }).limit(1).toArray();
  const latestWebhook = await db.collection("users").find({ when: { $exists: true } },
    { projection: { _id: 0, when: 1 } }).sort({ when: -1 }).limit(1).toArray();
  const earliestVisit = await db.collection("network-visits").find({ property: "aesthetic.computer" },
    { projection: { _id: 0, startedAt: 1 } }).sort({ startedAt: 1 }).limit(1).toArray();
  const traffic = await db.collection("network-visits").aggregate([
    { $match: { property: "aesthetic.computer", startedAt: { $gte: start, $lt: end }, automated: { $ne: true } } },
    { $group: { _id: null, visits: { $sum: 1 }, interacted: { $sum: { $cond: ["$interacted", 1, 0] } } } },
    { $project: { _id: 0 } },
  ]).toArray();
  const { SIGNUP_STAGES } = await import("../public/aesthetic.computer/lib/signup-model.mjs");
  const funnel = await db.collection("signup-attempts").aggregate([
    { $match: { startedAt: { $gte: start, $lt: end } } },
    { $group: { _id: { mode: "$mode", source: "$source", property: "$property", referrerHost: "$referrerHost" }, attempts: { $sum: 1 },
      ...Object.fromEntries(SIGNUP_STAGES.map(stage => [stage, { $sum: { $cond: [`$stages.${stage}`, 1, 0] } }])) } },
    { $sort: { attempts: -1 } },
  ]).toArray();
  const earliestAttempt = await db.collection("signup-attempts").find({}, { projection: { _id: 0, startedAt: 1 } }).sort({ startedAt: 1 }).limit(1).toArray();
  return {
    window: { start, end }, source: "Auth0 aesthetic tenant, current account records; handle:create server logs",
    total: summary(start, end),
    last24Hours: +end - +start >= 86400000 ? summary(new Date(+end - 86400000), end) : null,
    last7Days: +end - +start >= 7 * 86400000 ? summary(new Date(+end - 7 * 86400000), end) : null,
    previous7Days: +end - +start >= 14 * 86400000 ? summary(new Date(+end - 14 * 86400000), new Date(+end - 7 * 86400000)) : null,
    daily, latestHandle: latestHandle[0] || null,
    diagnostics: { latestWebhookSignup: latestWebhook[0]?.when || null,
      webhookBehindAccounts: accounts.some(row => new Date(row.created_at) > (latestWebhook[0]?.when || new Date(0))) },
    traffic: { ...(traffic[0] || { visits: 0, interacted: 0 }), earliestRetainedVisit: earliestVisit[0]?.startedAt || null },
    funnel: { earliestRetainedAttempt: earliestAttempt[0]?.startedAt || null, rows: funnel },
    notes: ["Account and handle completion reflect current records; deleted/merged accounts are not creation-event counts.",
      "Anonymous funnel attempts are client-reported, not unique people or authoritative account creations. Login, signup and handle-only attempts stay separate.",
      "Visits and attempts expire after 35 days; opt-outs, blocked requests and uninstrumented clients are absent. No pre-deployment funnel is inferred.",
      "Traffic is page visits, not unique visitors. Do not divide unrelated account totals by visits to claim a conversion rate."],
  };
}
