#!/usr/bin/env node
// Run on Lith: cd /opt/ac/system && node --env-file=.env ../toolchain/analytics/braincells-report.mjs --hours 24
// The Braincell economy for the network report: what is in circulation and
// what it is worth, who spent it on which models, what was bought, and AC's
// OpenRouter balance that pays for all of it. Read-only. Private: it names
// handles, because only the operator reads it. No prompts, outputs or
// payment identifiers leave the database.
import { connect, closePool } from "../../system/backend/database.mjs";
import { LEDGER_COLLECTION } from "../../system/backend/braincell-ledger.mjs";
import { createBalance } from "../../system/backend/openrouter-balance.mjs";
import { foldBraincells } from "./braincells-fold.mjs";

const args = process.argv.slice(2);
const option = name => args[args.indexOf(name) + 1];
const hours = args.includes("--hours") ? Number(option("--hours")) : 24;
const limit = args.includes("--limit") ? Number(option("--limit")) : 25;
if (!Number.isFinite(hours) || hours <= 0 || hours > 840) throw new Error("Use --hours 1..840");
if (!Number.isInteger(limit) || limit < 1 || limit > 200) throw new Error("Use --limit 1..200");
const end = new Date(), start = new Date(+end - hours * 3600_000), range = { $gte: start, $lt: end };
const days = [];
for (let d = new Date(start.toISOString().slice(0, 10)); d < end; d = new Date(+d + 86400_000)) days.push(d.toISOString().slice(0, 10));

async function openRouter() {
  const key = process.env.OPENROUTER_API_KEY;
  const balance = await createBalance({ key })();
  let usage = { unknown: true };
  if (key) try {
    const res = await fetch("https://openrouter.ai/api/v1/key", { headers: { Authorization: `Bearer ${key}` }, signal: AbortSignal.timeout(5000) });
    if (res.ok) {
      const { data } = await res.json();
      usage = { day_usd: data?.usage_daily, week_usd: data?.usage_weekly, month_usd: data?.usage_monthly, lifetime_usd: data?.usage, limit_usd: data?.limit ?? null };
    } else usage = { unknown: true, reason: `OpenRouter answered ${res.status}` };
  } catch { usage = { unknown: true, reason: "OpenRouter unreachable" }; }
  return { ...balance, keyUsage: usage, note: "One OpenRouter account pays every product's inference. Braincell sales never top it up; it is funded on openrouter.ai." };
}

async function stripePurchases() {
  if (!process.env.STRIPE_API_PRIV_KEY) return { available: false, reason: "no Stripe key on this host" };
  try {
    const { default: Stripe } = await import("stripe");
    const stripe = new Stripe(process.env.STRIPE_API_PRIV_KEY, { timeout: 20000, maxNetworkRetries: 1 });
    const seen = new Set(), users = [];
    let credits = 0, cents = 0;
    for (const type of ["checkout.session.completed", "checkout.session.async_payment_succeeded"]) {
      let after;
      do {
        const page = await stripe.events.list({ type, created: { gte: Math.floor(+start / 1000), lt: Math.ceil(+end / 1000) }, limit: 100, ...(after ? { starting_after: after } : {}) });
        for (const event of page.data) {
          const s = event.data.object;
          if (!event.livemode || s.metadata?.type !== "ac-credits" || s.payment_status !== "paid" || seen.has(s.id)) continue;
          seen.add(s.id); users.push(s.metadata.userSub); cents += s.amount_total || 0; credits += 1_000_000;
        }
        after = page.has_more && seen.size < 10000 ? page.data.at(-1)?.id : null;
      } while (after);
    }
    return { available: true, payments: seen.size, braincells: credits, usd: cents / 100, users };
  } catch { return { available: false, reason: "Stripe events unavailable" }; }
}

const { db } = await connect();
try {
  const opts = { maxTimeMS: 20000 };
  const [wallets, allowance, ledger, nopaint, speech, apple, tezos, stripe, openRouterState, firstLedger] = await Promise.all([
    db.collection("ac-credit-wallets").find({}, { projection: { balance: 1, spent: 1, holds: 1, daily: 1, grants: 1 }, ...opts }).toArray(),
    db.collection("ai-usage").find({ day: { $in: days } }, { projection: { handle: 1, day: 1, tokens: 1, asks: 1, lastModel: 1 }, ...opts }).toArray(),
    db.collection(LEDGER_COLLECTION).find({ at: range }, { projection: { _id: 0 }, ...opts }).toArray(),
    db.collection("nopaint-move-requests").find({ startedAt: range, status: "complete" }, { projection: { user: 1, model: 1, charged: 1, paid: 1 }, ...opts }).toArray(),
    db.collection("whistlegraph-speech-requests").find({ startedAt: range, status: "complete" }, { projection: { user: 1, charged: 1, paid: 1 }, ...opts }).toArray(),
    db.collection("ac-credit-purchases").find({ createdAt: range }, { projection: { user: 1, credits: 1, refundedAt: 1 }, ...opts }).toArray(),
    db.collection("ac-tezos-checkouts").find({ status: "credited", creditedAt: range }, { projection: { user: 1, credits: 1, usd: 1 }, ...opts }).toArray(),
    stripePurchases(),
    openRouter(),
    db.collection(LEDGER_COLLECTION).find({}, { projection: { at: 1 } }).sort({ at: 1 }).limit(1).next(),
  ]);
  // Subs to public handles, in one query for every sub the report touches.
  const subs = [...new Set([...wallets.map(w => w._id), ...nopaint.map(r => r.user), ...speech.map(r => r.user),
    ...apple.map(r => r.user), ...tezos.map(r => r.user), ...(stripe.users || [])].filter(Boolean))];
  const handles = Object.fromEntries((await db.collection("@handles").find({ _id: { $in: subs } }, { projection: { handle: 1 } }).toArray())
    .map(h => [h._id, `@${h.handle}`]));
  const report = foldBraincells({ wallets, allowance, ledger, nopaint, speech, apple, tezos, stripe, handles,
    firstLedgerAt: firstLedger?.at || null, openRouter: openRouterState }, { start, end, limit });
  console.log(JSON.stringify({ ...report, window: { ...report.window, hours } }, null, 2));
} finally { await closePool(); }
