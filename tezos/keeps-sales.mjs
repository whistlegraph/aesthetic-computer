#!/usr/bin/env node
// keeps-sales.mjs — read ACTUAL KidLisp keeps sales from the chain.
//
// Keeps sell through an objkt-style marketplace (ask -> fulfill_ask), NOT via
// objkt.com's own indexer — so objkt's public GraphQL shows zero sales for the
// collection. This reads the truth from TzKT instead:
//
//   A sale is a `transfer` on the keeps contract whose SENDER is the
//   marketplace contract and whose INITIATOR is the buyer. The price is the
//   amount of that buyer's `fulfill_ask` in the same block.
//
// Usage:
//   node tezos/keeps-sales.mjs                # recent 25 sales (mainnet)
//   node tezos/keeps-sales.mjs --limit=50
//   node tezos/keeps-sales.mjs --network=ghostnet
//   node tezos/keeps-sales.mjs --json         # machine-readable
//
// Dependency-free (Node 18+ global fetch).

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const __dir = dirname(fileURLToPath(import.meta.url));

// The marketplace contract keeps are listed/sold on (objkt-style ask/fulfill_ask).
const MARKETPLACE = "KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X";

const args = process.argv.slice(2);
const flag = (k) => args.includes(`--${k}`);
const opt = (k, d) => {
  const a = args.find((x) => x.startsWith(`--${k}=`));
  return a ? a.slice(k.length + 3) : d;
};
const LIMIT = Math.max(1, parseInt(opt("limit", "25"), 10) || 25);
const NETWORK = opt("network", "mainnet");
const AS_JSON = flag("json");

const apiBase = (net) =>
  net === "mainnet" ? "https://api.tzkt.io" : `https://api.${net}.tzkt.io`;

function activeContract(net) {
  const j = JSON.parse(readFileSync(join(__dir, "contracts.json"), "utf8"));
  const addr = j?.activeContracts?.[net]?.keeps;
  if (!addr) throw new Error(`no active keeps contract for network "${net}"`);
  return addr;
}

async function jget(url) {
  const r = await fetch(url);
  if (!r.ok) throw new Error(`${r.status} ${r.statusText} — ${url}`);
  return r.json();
}

// The FA2 transfer parameter: [{ from_, txs: [{ to_, token_id, amount }] }].
function parseTransfer(param) {
  try {
    const batch = Array.isArray(param?.value) ? param.value : [];
    const from = batch[0]?.from_;
    const tx = batch[0]?.txs?.[0] || {};
    return { from, to: tx.to_, tokenId: String(tx.token_id) };
  } catch {
    return {};
  }
}

async function main() {
  const A = apiBase(NETWORK);
  const contract = activeContract(NETWORK);

  // 1. Every marketplace-mediated transfer of a keeps token IS a sale.
  const saleOps = await jget(
    `${A}/v1/operations/transactions` +
      `?target=${contract}&entrypoint=transfer&sender=${MARKETPLACE}` +
      `&sort.desc=level&limit=10000`,
  );
  const totalSales = saleOps.length;
  const recent = saleOps.slice(0, LIMIT);

  // 2. Token names, in one call (id -> "$piece").
  const toks = await jget(
    `${A}/v1/tokens?contract=${contract}&limit=10000&select=tokenId,metadata`,
  );
  const nameById = new Map(
    toks.map((t) => [String(t.tokenId), t.metadata?.name || `#${t.tokenId}`]),
  );

  // 3. Resolve each sale's price + settlement path. The buyer is always the
  //    transfer RECIPIENT (to_); the initiator is whoever triggered the settle
  //    — the buyer for `fulfill_ask[_bulk]`, the seller for `fulfill_offer`.
  //    Exact price is clean for a single `fulfill_ask`; a multi-item bulk or an
  //    offer can't be cleanly attributed per-token, so we label rather than guess.
  const sales = [];
  for (const op of recent) {
    const { from, to, tokenId } = parseTransfer(op.parameter);
    const initiator = op.initiator?.address;
    let priceXtz = null;
    let via = null;
    try {
      const calls = await jget(
        `${A}/v1/operations/transactions?sender=${initiator}&level=${op.level}&target=${MARKETPLACE}`,
      );
      const SETTLE = ["fulfill_ask", "fulfill_ask_bulk", "fulfill_offer"];
      const settle = calls.find((c) => SETTLE.includes(c.parameter?.entrypoint));
      const ep = settle?.parameter?.entrypoint;
      if (ep === "fulfill_ask") {
        priceXtz = settle.amount / 1e6;
        via = "ask";
      } else if (ep === "fulfill_ask_bulk") {
        // Only trust the amount if this buyer took exactly one keep in the block.
        const sib = await jget(
          `${A}/v1/operations/transactions?target=${contract}&entrypoint=transfer` +
            `&sender=${MARKETPLACE}&level=${op.level}`,
        );
        const mine = sib.filter((s) => s.initiator?.address === initiator);
        if (mine.length === 1) {
          priceXtz = settle.amount / 1e6;
          via = "bulk";
        } else {
          via = `bulk×${mine.length}`;
        }
      } else if (ep === "fulfill_offer") {
        via = "offer";
      } else if (ep) {
        via = ep;
      }
    } catch {
      /* leave price null */
    }
    sales.push({
      timestamp: op.timestamp,
      tokenId,
      piece: nameById.get(tokenId) || `#${tokenId}`,
      seller: from,
      buyer: to,
      priceXtz,
      via,
    });
  }

  // 4. Resolve buyer .tez domains in one batch.
  const addrs = [...new Set(sales.map((s) => s.buyer).filter(Boolean))];
  const nameByAddr = new Map();
  if (addrs.length) {
    try {
      // One address can own several .tez names; prefer its reverse (primary) record.
      const doms = await jget(`${A}/v1/domains?address.in=${addrs.join(",")}&limit=1000`);
      for (const d of doms) {
        const a = d.address?.address || d.address;
        if (!a) continue;
        if (d.reverse || !nameByAddr.has(a)) nameByAddr.set(a, d.name);
      }
    } catch {
      /* domains are best-effort */
    }
  }
  const who = (a) => nameByAddr.get(a) || (a ? a.slice(0, 8) + "…" : "?");

  const priced = sales.filter((s) => s.priceXtz != null);
  const recentVolume = priced.reduce((n, s) => n + s.priceXtz, 0);

  if (AS_JSON) {
    console.log(
      JSON.stringify(
        { contract, network: NETWORK, totalSales, shown: sales.length, recentVolumeXtz: recentVolume, sales },
        null,
        2,
      ),
    );
    return;
  }

  console.log(`\n🔮 KidLisp Keeps — sales  (${NETWORK})`);
  console.log(`   contract ${contract}`);
  console.log(`   ${totalSales} sales all-time · showing latest ${sales.length}\n`);
  const pad = (s, n) => String(s).padEnd(n);
  const padL = (s, n) => String(s).padStart(n);
  console.log(
    `   ${pad("date", 17)}${pad("piece", 9)}${padL("price", 9)}   buyer`,
  );
  console.log(`   ${"─".repeat(17)}${"─".repeat(9)}${"─".repeat(9)}   ${"─".repeat(16)}`);
  for (const s of sales) {
    const date = s.timestamp.slice(0, 16).replace("T", " ");
    const price =
      s.priceXtz != null ? `${s.priceXtz.toFixed(2)}ꜩ` : s.via || "—";
    const tag = s.priceXtz != null && s.via && s.via !== "ask" ? ` (${s.via})` : "";
    console.log(
      `   ${pad(date, 17)}${pad(s.piece, 9)}${padL(price, 9)}   ${who(s.buyer)}${tag}`,
    );
  }
  if (priced.length) {
    const prices = priced.map((s) => s.priceXtz);
    console.log(
      `\n   latest ${priced.length}: ${recentVolume.toFixed(2)}ꜩ total · ` +
        `${(recentVolume / priced.length).toFixed(2)}ꜩ avg · ` +
        `${Math.min(...prices).toFixed(2)}–${Math.max(...prices).toFixed(2)}ꜩ range`,
    );
  }
  console.log("");
}

main().catch((e) => {
  console.error("keeps-sales error:", e.message);
  process.exit(1);
});
