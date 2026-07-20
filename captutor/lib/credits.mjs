// credits — how much of the client's money this run is allowed to spend.
//
//   node captutor.mjs balance <screenplay>     what Iris has right now
//
// Iris's Fuser balance (the `24,111✦` bottom-left of the workspace) is REAL
// MONEY on a client's production account. Generating an app or an image debits
// it. So an unattended renderer — the whole point of lib/login.mjs — is also a
// thing that can quietly spend, and a retry loop or a scripted sweep across
// locales could drain the account with nobody in the room.
//
// Three guards, in increasing order of how much they hurt:
//
//   WARN  (default 2000) — loud, but the take proceeds.
//   FLOOR (default 1000) — `render` REFUSES. A take shot with too few credits
//         produces a broken tutorial anyway (the generation fails on camera), so
//         failing before the camera rolls is strictly better than after.
//   CAP   (default 5 takes/hour) — the backstop that does not depend on anyone
//         reading the other two. It is persisted, not in-process, because the
//         thing we are actually afraid of is a shell loop calling captutor five
//         hundred times, and an in-process counter would not see that at all.
//
// All three are env-overridable, and the ledger is a plain JSON file you can
// delete. These are a floor under a robot, not a lock against a person.
//
// The balance is read from `account.getQuotas` — fuser's own tRPC route, called
// from INSIDE the page, so the session cookie rides along and there is no token
// to keep anywhere. It is also published over REST (trpc-to-openapi) at
// /api/v1/account/getQuotas, which looks nicer and does not work: that path is
// not in the API's CORS allowlist, so from app.fuser.studio it is a flat
// "Failed to fetch". The tRPC path is the one the app itself uses.
//
// The DOM scrape stays as a fallback, because the number on screen is formatted
// for humans ("24,111", "1.2M") and is a threshold, never an accounting figure.

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LEDGER = join(HERE, "..", "out", "takes.json");

const num = (v, d) => (Number.isFinite(+v) && v !== "" ? +v : d);

export const WARN_BELOW = num(process.env.CAPTUTOR_CREDIT_WARN, 2000);
export const FLOOR = num(process.env.CAPTUTOR_CREDIT_FLOOR, 1000);
export const MAX_TAKES = num(process.env.CAPTUTOR_MAX_TAKES, 5);
export const WINDOW_MIN = num(process.env.CAPTUTOR_TAKE_WINDOW_MIN, 60);

/// What Iris can actually spend. Org credits (she is in the Fuser org) are the
/// pool that gets debited when an org is active, so the spendable figure is the
/// org's when there is one — not the personal balance, which can read healthy
/// while the pool that actually pays is empty.
export async function readCredits(cdp) {
  const api = await cdp.eval(`(async () => {
    // Same derivation fuser uses (packages/core/src/config.ts): app.<domain> →
    // api.<domain>. Asking the page for its own origin means this keeps working
    // on staging without a second constant to forget about.
    const api = location.origin.replace(/\\/\\/app[.-]?/, "//api.");
    try {
      const r = await fetch(api + "/api/v1/trpc/account.getQuotas", {
        credentials: "include", headers: { accept: "application/json" },
      });
      if (!r.ok) return { ok: false, status: r.status };
      const j = await r.json();
      // superjson: the payload is under result.data.json. Tolerate a plain
      // transformer-less shape too, in case that ever changes.
      const q = j?.result?.data?.json ?? j?.result?.data ?? j;
      if (typeof q?.credits !== "number") return { ok: false, shape: Object.keys(q || {}).slice(0, 8) };
      return {
        ok: true, credits: q.credits, orgCredits: q.orgCredits ?? null,
        spendLimitRemaining: q.spendLimitRemaining ?? null, tier: q.effectiveTier ?? null,
      };
    } catch (e) { return { ok: false, err: String(e).slice(0, 120) }; }
  })()`);

  if (api?.ok) {
    const spendable = api.orgCredits ?? api.credits;
    return { ...api, spendable, source: "getQuotas" };
  }

  // Fallback: the number the user is looking at. Rounded and comma'd, so it is
  // good enough for a threshold and never for accounting.
  const dom = await cdp.eval(`(() => {
    const m = (document.body.innerText || "").match(/([\\d][\\d,.]*\\s*[KMB]?)\\s*✦/);
    if (!m) return null;
    const s = m[1].replace(/,/g, "").trim();
    const mult = /K$/i.test(s) ? 1e3 : /M$/i.test(s) ? 1e6 : /B$/i.test(s) ? 1e9 : 1;
    const n = parseFloat(s) * mult;
    return Number.isFinite(n) ? Math.round(n) : null;
  })()`);
  if (dom == null) {
    throw new Error(
      `could not read Iris's credit balance (getQuotas said ${JSON.stringify(api)}, ` +
      `and no ✦ figure is on screen). Is she signed in?`);
  }
  return { ok: true, credits: dom, orgCredits: null, spendable: dom, source: "dom" };
}

export const fmt = (n) => (n == null ? "—" : n.toLocaleString("en-US"));

// ── the ledger ─────────────────────────────────────────────────────────────
// Every take, what it cost, and when. Nobody knew what a video cost before this
// existed; now it is one file.

function ledger() {
  if (!existsSync(LEDGER)) return [];
  try { return JSON.parse(readFileSync(LEDGER, "utf8")); } catch { return []; }
}

function writeLedger(rows) {
  mkdirSync(dirname(LEDGER), { recursive: true });
  writeFileSync(LEDGER, JSON.stringify(rows.slice(-500), null, 2));
}

export function recordTake(row) {
  const rows = ledger();
  rows.push({ at: new Date().toISOString(), ...row });
  writeLedger(rows);
  return LEDGER;
}

export function recentTakes(minutes = WINDOW_MIN) {
  const cut = Date.now() - minutes * 60_000;
  return ledger().filter((r) => Date.parse(r.at) >= cut);
}

/**
 * Stand between a `render` and the client's money. Throws to refuse; returns the
 * balance to proceed with.
 */
export async function guard(cdp, { slug, locale, format }) {
  const bal = await readCredits(cdp);
  const spendable = bal.spendable;

  const recent = recentTakes();
  if (recent.length >= MAX_TAKES) {
    throw new Error(
      `take cap: ${recent.length} takes already in the last ${WINDOW_MIN} min ` +
      `(max ${MAX_TAKES}).\n` +
      `  Every take can spend Iris's credits, and this cap is the backstop against a\n` +
      `  loop nobody is watching. Raise it deliberately if you mean to:\n` +
      `    CAPTUTOR_MAX_TAKES=10 node captutor.mjs render ${slug}`);
  }

  if (spendable < FLOOR) {
    throw new Error(
      `REFUSING to record: Iris has ${fmt(spendable)}✦, below the floor of ${fmt(FLOOR)}✦.\n` +
      `  Generations would fail mid-take and the tutorial would film the failure.\n` +
      `  Top her up, or lower the floor on purpose: CAPTUTOR_CREDIT_FLOOR=…`);
  }

  if (spendable < WARN_BELOW) {
    console.log(`\n${"!".repeat(60)}`);
    console.log(`!! LOW CREDITS — Iris has ${fmt(spendable)}✦ (warn below ${fmt(WARN_BELOW)}✦).`);
    console.log(`!! A generating take may run dry on camera. Top her up soon.`);
    console.log(`${"!".repeat(60)}\n`);
  } else {
    console.log(`  ✦ ${fmt(spendable)} credits (${bal.source}${bal.orgCredits != null ? ", org pool" : ""})`);
  }

  return bal;
}

/// Close the books on a take: what it cost, on the record.
export async function settle(cdp, before, meta) {
  let after = null;
  try { after = await readCredits(cdp); } catch { /* the browser may be gone */ }
  const spent = after ? before.spendable - after.spendable : null;
  recordTake({ ...meta, before: before.spendable, after: after?.spendable ?? null, spent });
  if (after) {
    console.log(`  ✦ ${fmt(before.spendable)} → ${fmt(after.spendable)}` +
      `  (this take cost ${spent === 0 ? "nothing" : `${fmt(spent)}✦`})`);
  }
  return { after, spent };
}
