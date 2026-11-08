#!/usr/bin/env node
// readers-xref.mjs — who has been reading Sotce Net, with identities.
//
// For every signed-in reader the account-activity log saw in the window:
// Sotce handle, Auth0 profile (email, name, provider, sign-up date, logins),
// touches / questions / chat counts, Stripe customer and subscription, and
// the sister aesthetic.computer account (same verified email) with its handle
// and recent activity. Operator-only: emails and names leave the database.
// Read-only.
//
// Runs on lith:
//   cd /opt/ac/system && node --env-file=.env ../toolchain/sotce/readers-xref.mjs --days 14
// or from a copy anywhere on lith with AC_SYSTEM=/opt/ac/system.
import { createRequire } from "node:module";

const SYSTEM = process.env.AC_SYSTEM || new URL("../../system", import.meta.url).pathname;
const require = createRequire(SYSTEM + "/package.json");
const Stripe = require("stripe");
const { connect, closePool } = await import(SYSTEM + "/backend/database.mjs");
const { prodProductId } = await import(SYSTEM + "/backend/sotce-net-constants.mjs");

const args = process.argv.slice(2);
const option = name => args[args.indexOf(name) + 1];
const days = args.includes("--days") ? Number(option("--days")) : 14;
if (!Number.isInteger(days) || days < 1 || days > 90) throw new Error("Use --days 1..90");
const end = new Date(), start = new Date(+end - days * 86400_000);

// 🔐 Auth0 management reads, one token per tenant.
const TENANTS = {
  sotce: { base: "https://sotce.us.auth0.com", id: process.env.SOTCE_AUTH0_M2M_CLIENT_ID, secret: process.env.SOTCE_AUTH0_M2M_SECRET },
  aesthetic: { base: "https://aesthetic.us.auth0.com", id: process.env.AUTH0_M2M_CLIENT_ID, secret: process.env.AUTH0_M2M_SECRET },
};
const tokens = {};
async function token(tenant) {
  if (tokens[tenant]) return tokens[tenant];
  const { base, id, secret } = TENANTS[tenant];
  const res = await fetch(`${base}/oauth/token`, { method: "POST", headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ client_id: id, client_secret: secret, audience: `${base}/api/v2/`, grant_type: "client_credentials" }) });
  if (!res.ok) throw new Error(`${tenant} token ${res.status}`);
  return (tokens[tenant] = (await res.json()).access_token);
}
async function auth0(tenant, path) {
  const res = await fetch(`${TENANTS[tenant].base}/api/v2/${path}`, { headers: { Authorization: `Bearer ${await token(tenant)}` } });
  if (res.status === 429) { await new Promise(r => setTimeout(r, 1500)); return auth0(tenant, path); }
  if (!res.ok) return null;
  return res.json();
}
const profile = user => user && {
  email: user.email, emailVerified: user.email_verified, name: user.name !== user.email ? user.name : undefined,
  nickname: user.nickname, provider: user.identities?.[0]?.provider, signedUp: user.created_at, lastLogin: user.last_login, logins: user.logins_count,
};

const { db } = await connect();
try {
  // 📖 Every Sotce reader active in the window, with all-time first sighting.
  const readers = await db.collection("account-activity").aggregate([
    { $match: { tenant: "sotce", at: { $gte: start, $lt: end } } },
    { $group: { _id: "$user", events: { $sum: 1 }, sessions: { $addToSet: "$session" }, lastSeen: { $max: "$at" },
      days: { $addToSet: { $dateToString: { date: "$at", format: "%Y-%m-%d" } } }, actions: { $addToSet: "$action" },
      referrers: { $addToSet: "$referrerHost" } } },
    { $lookup: { from: "account-activity", let: { user: "$_id" }, as: "first", pipeline: [
      { $match: { $expr: { $and: [{ $eq: ["$tenant", "sotce"] }, { $eq: ["$user", "$$user"] }] } } },
      { $sort: { at: 1 } }, { $limit: 1 }, { $project: { _id: 0, at: 1 } }] } },
    { $project: { events: 1, sessions: { $size: "$sessions" }, lastSeen: 1, days: { $size: "$days" }, actions: 1,
      referrers: { $filter: { input: "$referrers", cond: { $ne: ["$$this", null] } } }, firstSeen: { $first: "$first.at" } } },
    { $sort: { lastSeen: -1 } },
  ], { maxTimeMS: 30000 }).toArray();

  const subs = readers.map(r => r._id);
  const handles = new Map((await db.collection("@handles").find({ _id: { $in: subs.map(s => "sotce-" + s) } }).toArray()).map(r => [String(r._id).slice(6), r.handle]));
  const count = async (name, field = "user") => new Map((await db.collection(name).aggregate([{ $match: { [field]: { $in: subs } } }, { $group: { _id: "$" + field, n: { $sum: 1 } } }]).toArray()).map(r => [r._id, r.n]));
  const [touches, asks, chat] = await Promise.all([count("sotce-touches"), count("sotce-asks"), count("chat-sotce")]);

  const stripeKey = (process.env.SOTCE_STRIPE_API_PRIV_KEY || "").replace(/^["']|["']$/g, "");
  const stripe = stripeKey ? new Stripe(stripeKey) : null;
  async function subscription(sub) {
    if (!stripe) return { unknown: "SOTCE_STRIPE_API_PRIV_KEY missing" };
    try {
      const { data } = await stripe.customers.search({ query: `metadata['sub']:'${sub}'`, limit: 1 });
      const customer = data[0];
      if (!customer) return null;
      const list = await stripe.subscriptions.list({ customer: customer.id, status: "all", limit: 10 });
      const ours = list.data.filter(s => s.items.data.some(i => i.price.product === prodProductId)).sort((a, b) => b.created - a.created);
      const current = ours[0];
      return { name: customer.name || undefined, email: customer.email, customerSince: new Date(customer.created * 1000).toISOString().slice(0, 10),
        status: current?.status || "none", since: current ? new Date(current.created * 1000).toISOString().slice(0, 10) : undefined,
        endingAtPeriodEnd: current?.cancel_at_period_end || undefined, subscriptions: ours.length };
    } catch (e) { return { unknown: String(e?.message || e).slice(0, 120) }; }
  }

  // 🎨 The sister aesthetic.computer account: same verified email.
  async function aesthetic(email, verified) {
    if (!email || !verified) return null;
    const users = await auth0("aesthetic", `users-by-email?email=${encodeURIComponent(email)}`);
    const user = users?.[0];
    if (!user) return null;
    const sub = user.user_id;
    const [handle, activity] = await Promise.all([
      db.collection("@handles").findOne({ _id: sub }, { projection: { handle: 1 } }),
      db.collection("account-activity").aggregate([
        { $match: { tenant: "aesthetic", user: sub } },
        { $group: { _id: null, events: { $sum: 1 }, lastAt: { $max: "$at" }, pieces: { $addToSet: "$piece" }, properties: { $addToSet: "$property" } } },
      ]).next(),
    ]);
    return { sub, handle: handle ? "@" + handle.handle : undefined, ...profile(user),
      events: activity?.events || 0, lastActive: activity?.lastAt, pieces: activity?.pieces?.filter(Boolean).slice(0, 20), properties: activity?.properties?.filter(Boolean) };
  }

  const rows = [];
  for (const r of readers) {
    const user = await auth0("sotce", `users/${encodeURIComponent(r._id)}`);
    const p = profile(user) || {};
    rows.push({
      sub: r._id, handle: handles.has(r._id) ? "@" + handles.get(r._id) : undefined, ...p,
      window: { events: r.events, sessions: r.sessions, days: r.days, actions: r.actions, referrers: r.referrers, lastSeen: r.lastSeen },
      firstSeen: r.firstSeen, touches: touches.get(r._id) || 0, asks: asks.get(r._id) || 0, chat: chat.get(r._id) || 0,
      stripe: await subscription(r._id),
      aesthetic: await aesthetic(p.email, p.emailVerified),
    });
  }

  console.log(JSON.stringify({
    format: "sotce.readers-xref.v1", start, end, days, readers: rows.length,
    withAestheticAccount: rows.filter(r => r.aesthetic).length, withAestheticHandle: rows.filter(r => r.aesthetic?.handle).length,
    rows,
    notes: {
      readers: "Signed-in readers from the account-activity log (tenant sotce) in the window; signed-out reading is invisible.",
      aesthetic: "Sister account = an aesthetic.computer Auth0 user with the same verified email; its handle, all-time server-verified activity and pieces.",
      stripe: "Customer matched by metadata.sub; status is the newest Sotce Net subscription. none = customer exists without one.",
    },
  }, null, 1));
} finally { await closePool(); }
