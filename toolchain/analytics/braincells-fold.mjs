// braincells-fold — turn raw Braincell records into the network report's
// shape. Pure: braincells-report.mjs fetches, this folds, tests feed fixtures.
import { BRAINCELLS_PER_USD, INFERENCE_MARKUP } from "../../system/backend/easel-paid-credits.mjs";

export const usd = cells => Math.round(cells / BRAINCELLS_PER_USD * 100) / 100;
const money = cells => ({ braincells: cells, usd: usd(cells) });
export const grantKind = id => id.startsWith("cs_") ? "stripe" : id.startsWith("apple:") ? "apple" : id.startsWith("tezos:") ? "tezos" : "staff";

// UTC days a window touches; wallet and allowance totals are kept per day.
export function windowDays(start, end) {
  const days = [];
  for (let d = new Date(start.toISOString().slice(0, 10)); d < end; d = new Date(+d + 86400_000)) days.push(d.toISOString().slice(0, 10));
  return days;
}

export function foldBraincells(raw, { start, end, limit = 25 }) {
  const { wallets = [], allowance = [], ledger = [], nopaint = [], speech = [], apple = [], tezos = [],
    stripe = { available: false }, handles = {}, firstLedgerAt = null, openRouter = null } = raw;
  const days = windowDays(start, end);
  const who = sub => handles[sub] || (sub ? "(no handle)" : "(unknown)");
  const add = (map, key, cells, extra = {}) => {
    const row = map[key] ??= { braincells: 0, requests: 0 };
    row.braincells += cells; row.requests += extra.requests ?? 1;
    for (const [k, v] of Object.entries(extra)) if (k !== "requests") row[k] = typeof v === "number" ? (row[k] || 0) + v : v;
  };
  const top = map => Object.entries(map).sort((a, b) => b[1].braincells - a[1].braincells).slice(0, limit);

  // 💰 Circulation: what the wallets hold now.
  let held = 0, debt = 0, reserved = 0, lifetimeSpent = 0, funded = 0;
  const grants = {}, holders = {}, paidByUser = {};
  for (const w of wallets) {
    const balance = Number(w.balance) || 0;
    if (balance > 0) { held += balance; funded++; add(holders, who(w._id), balance, { requests: 0 }); }
    if (balance < 0) debt -= balance;
    reserved += Object.values(w.holds || {}).reduce((n, h) => n + (Number(h.amount) || 0), 0);
    lifetimeSpent += Number(w.spent) || 0;
    for (const id of w.grants || []) { const kind = grantKind(String(id)); grants[kind] = (grants[kind] || 0) + 1; }
    const spent = days.reduce((n, d) => n + (Number(w.daily?.[d]) || 0), 0);
    if (spent) add(paidByUser, who(w._id), spent, { requests: 0 });
  }

  // 🧠 Spending in the window, per request, wherever a model is recorded.
  const byModel = {}, byHandle = {}, byProduct = {};
  const spend = (product, handle, model, cells, paid) => {
    if (!cells) return;
    const extra = { paidBraincells: paid ? cells : 0 };
    add(byModel, model || "unknown", cells, extra); add(byHandle, handle, cells, extra); add(byProduct, product, cells, extra);
  };
  for (const r of ledger) spend(r.product, r.handle ? `@${r.handle}` : who(r.user), r.model, r.braincells, r.paid);
  for (const r of nopaint) spend("nopaint", who(r.user), r.model, Number(r.charged) || 0, (Number(r.paid) || 0) > 0);
  for (const r of speech) spend("whistlegraph-speech", who(r.user), "openai/whisper-1", Number(r.charged) || 0, (Number(r.paid) || 0) > 0);

  const allowanceByHandle = {};
  for (const r of allowance) add(allowanceByHandle, `@${r.handle}`, Number(r.tokens) || 0, { requests: Number(r.asks) || 0, lastModel: r.lastModel || "" });
  const allowanceTotal = Object.values(allowanceByHandle).reduce((n, r) => n + r.braincells, 0);
  const paidTotal = Object.values(paidByUser).reduce((n, r) => n + r.braincells, 0);
  const sum = (rows, key) => rows.reduce((n, r) => n + (Number(r[key]) || 0), 0);

  return {
    format: "ac.braincells.v1", window: { start, end, utcDays: days },
    circulation: {
      held: money(held), reservedInFlight: money(reserved), debt: money(debt), lifetimeSpent: money(lifetimeSpent),
      wallets: wallets.length, fundedWallets: funded, grantsByKind: grants,
      topHolders: top(holders).map(([handle, v]) => ({ handle, ...money(v.braincells) })),
      valueNote: `Valued at the pack price: ${BRAINCELLS_PER_USD.toLocaleString("en-US")} Braincells per US dollar ($5 per million). Staff grants count at that price though nobody paid for them.`,
    },
    spending: {
      totals: {
        purchasedSpent: { ...money(paidTotal), basis: "wallet spend on the UTC days above, every product" },
        freeAllowanceUsed: { ...money(allowanceTotal), basis: "daily free allowance on the UTC days above; AC's cost, never sold" },
        estimatedProviderCostUsd: Math.round(usd(paidTotal + allowanceTotal) / INFERENCE_MARKUP * 100) / 100,
        costNote: `Hosted inference bills at ${INFERENCE_MARKUP}x OpenRouter's cost, so provider cost is about value / ${INFERENCE_MARKUP}.`,
      },
      byModel: top(byModel).map(([model, v]) => ({ model, ...v, usd: usd(v.braincells) })),
      byHandle: top(byHandle).map(([handle, v]) => ({ handle, ...v, usd: usd(v.braincells) })),
      byProduct: top(byProduct).map(([product, v]) => ({ product, ...v, usd: usd(v.braincells) })),
      purchasedByHandle: top(paidByUser).map(([handle, v]) => ({ handle, ...money(v.braincells) })),
      allowanceByHandle: top(allowanceByHandle).map(([handle, v]) => ({ handle, braincells: v.braincells, asks: v.requests, lastModel: v.lastModel })),
      coverage: {
        modelLedgerSince: firstLedgerAt,
        sources: ["braincell-spends: Aesel and Whistlegraph hosted inference, per request, from the ledger rollout",
          "nopaint-move-requests: complete moves", "whistlegraph-speech-requests: complete transcriptions"],
        gaps: "Choir's curator and /api/ask keep no per-request model rows; their spend shows only in the wallet and allowance totals.",
      },
    },
    purchases: {
      stripe: stripe.available ? { payments: stripe.payments, ...money(stripe.braincells), chargedUsd: stripe.usd, buyers: [...new Set(stripe.users.map(who))],
        ...(stripe.source ? { source: stripe.source } : {}), ...(stripe.note ? { note: stripe.note } : {}) } : stripe,
      apple: { purchases: apple.length, ...money(sum(apple, "credits")), refunded: apple.filter(r => r.refundedAt).length,
        buyers: [...new Set(apple.map(r => who(r.user)))], note: "Apple keeps its share; valued at pack price." },
      tezos: { payments: tezos.length, ...money(sum(tezos, "credits")), chargedUsd: sum(tezos, "usd"), buyers: [...new Set(tezos.map(r => who(r.user)))] },
    },
    openRouter,
  };
}
