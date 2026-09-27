#!/usr/bin/env node
// crypto-mcp.mjs — AC's wallets, the Tezos art market and the daily token.
//
// crypto_wallets and crypto_market read public chain data only (TzKT,
// Blockscout, Solana RPC, the objkt indexer, CoinGecko). daily_token_status
// reads the chain for the minted dailies and, when jasellite answers, its
// nightly log. daily_token_run re-runs a date on jasellite (confirm: true).
//
// Nothing here moves money. crypto_transfer_prepare checks a transfer or a
// Keeps fee withdrawal and returns the exact command; @jeffrey running it is
// the approval. No tool reads a key.

import { execFile } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";

const ROOT = join(dirname(fileURLToPath(import.meta.url)), "../..");
const TZKT = "https://api.tzkt.io/v1";
const OBJKT = "https://data.objkt.com/v3/graphql";

// Public addresses only (mirrors vault/banking/crypto/wallets.csv).
const TEZOS = {
  aesthetic: "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE", // aesthetic.tez — daily token signer
  keeps: "tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC", // keeps.tez — Keeps admin + royalty receiver
  kidlisp: "tz1fEjGQrEE2LXNKqcpTYAJV16pbbFpLeNyd", // kidlisp treasury
  staging: "tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt",
};
const EVM = {
  "4esthetic.eth": "0x5e6758C96A4cB5E2A1FE2E2772020dc8ad753b08",
  "citizen idx1": "0xF637C4E11072C46A3bF7cF70ef32470562B5a479",
  "whistlegraph.eth": "0x238c9c645c6EE83d4323A2449C706940321a0cBf",
  "eth-unknown": "0x98eAc86755792e03D0f027cA8CcFa83818B994c4",
};
const SOLANA = {
  phantom: "D5tLrs4Ubh3tcHxhSmorPmGrDsozuSLhQDGVoBbR5P9d",
  "axio trade": "6JRph4ZZf5jt17CZhyBM9SYVexLrKmyPVMGCTMA4A7Ps",
};
const KEEPS = "KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB";
const HEN = "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
const DAILY_BURN_XTZ = 0.06; // a mint + a listing
const DAILY_FLOOR_XTZ = 0.15; // daily-token.mjs refuses to sign below this
// Every hic et nunc mint by aesthetic.tez from here on is a daily token (the
// last manual one was 2025-11); names are just the episode title.
const DAILY_SINCE = "2026-09-26";

const get = async (url, opts = {}) => {
  const r = await fetch(url, { ...opts, signal: AbortSignal.timeout(opts.timeout ?? 30_000) });
  if (!r.ok) throw new Error(`${r.status} ${url.split("?")[0]}`);
  return r.json();
};
const objkt = async (query) => {
  const j = await get(OBJKT, { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify({ query }) });
  if (j.errors) throw new Error(`objkt: ${j.errors[0]?.message}`);
  return j.data;
};
const settle = async (p) => { try { return await p; } catch (e) { return { error: String(e.message || e) }; } };
const round = (n, d = 4) => Math.round(n * 10 ** d) / 10 ** d;
const tez = (mutez) => Number(mutez) / 1e6;

async function prices() {
  const j = await settle(get("https://api.coingecko.com/api/v3/simple/price?ids=tezos,ethereum,solana&vs_currencies=usd"));
  return { xtz: j.tezos?.usd, eth: j.ethereum?.usd, sol: j.solana?.usd };
}

// 👛 Wallets

async function wallets() {
  const usd = await prices();
  const tezos = {};
  for (const [name, address] of Object.entries(TEZOS)) {
    tezos[name] = { address, xtz: round(tez(await get(`${TZKT}/accounts/${address}/balance`)), 6) };
  }
  const keepsXtz = tez(await get(`${TZKT}/accounts/${KEEPS}/balance`));
  const offers = await settle(objkt(`{ offer_active(where:{token:{fa_contract:{_eq:"${KEEPS}"}}}, order_by:{price:desc}, limit:10){ price token{token_id name} buyer_address } }`));
  const listings = await settle(objkt(`{ listing_active(where:{token:{fa_contract:{_eq:"${KEEPS}"}}}, order_by:{price:asc}, limit:20){ price amount_left token{token_id name} seller_address } }`));

  const evm = {};
  for (const [name, address] of Object.entries(EVM)) {
    const row = {};
    for (const [chain, host] of [["ethereum", "eth.blockscout.com"], ["base", "base.blockscout.com"]]) {
      const a = await settle(get(`https://${host}/api/v2/addresses/${address}`));
      row[chain] = a.error ? a : round(Number(a.coin_balance || 0) / 1e18, 6);
    }
    evm[name] = { address, ...row };
  }

  const solana = {};
  for (const [name, address] of Object.entries(SOLANA)) {
    const r = await settle(get("https://api.mainnet-beta.solana.com", {
      method: "POST", headers: { "content-type": "application/json" },
      body: JSON.stringify({ jsonrpc: "2.0", id: 1, method: "getBalance", params: [address] }),
    }));
    solana[name] = { address, sol: r.error ? r : round((r.result?.value || 0) / 1e9, 6) };
  }

  const xtzTotal = Object.values(tezos).reduce((a, w) => a + w.xtz, 0);
  const ethTotal = Object.values(evm).reduce((a, w) => a + (+w.ethereum || 0) + (+w.base || 0), 0);
  const solTotal = Object.values(solana).reduce((a, w) => a + (+w.sol || 0), 0);
  return {
    prices: usd,
    tezos, keepsContract: { address: KEEPS, withdrawableXtz: keepsXtz },
    keepsMarket: {
      offers: offers.error ? offers : offers.offer_active.map((o) => ({ token: `#${o.token.token_id} ${o.token.name}`, xtz: tez(o.price), buyer: o.buyer_address })),
      listings: listings.error ? listings : listings.listing_active.map((l) => ({ token: `#${l.token.token_id} ${l.token.name}`, xtz: tez(l.price), left: l.amount_left, seller: l.seller_address })),
    },
    evm, solana,
    totalsUsd: {
      tezos: usd.xtz && round((xtzTotal + keepsXtz) * usd.xtz, 2),
      evm: usd.eth && round(ethTotal * usd.eth, 2),
      solana: usd.sol && round(solTotal * usd.sol, 2),
    },
    dailyTokenGas: runway(tezos.aesthetic.xtz),
  };
}

const runway = (xtz) => ({ xtz, daysLeft: Math.max(0, Math.floor((xtz - DAILY_FLOOR_XTZ) / DAILY_BURN_XTZ)) });

// 📈 Market

async function market({ days = 7, top = 15 } = {}) {
  days = Number(days);
  if (!(days > 0 && days <= 30)) throw new Error("days must be 1..30");
  const since = new Date(Date.now() - days * 864e5).toISOString();
  const fields = "price buyer_address seller_address token{ token_id name creators{creator_address holder{alias}} fa{name contract} }";
  const sales = [];
  for (const table of ["listing_sale", "offer_sale"]) {
    for (let offset = 0; offset < 40_000; offset += 500) {
      const batch = (await objkt(`{ ${table}(where:{timestamp:{_gte:"${since}"}}, order_by:{id:asc}, limit:500, offset:${offset}){ ${fields} } }`))[table];
      sales.push(...batch);
      if (batch.length < 500) break;
    }
  }

  // Wash filter: a creator whose repeated sales (3+, 500+ XTZ together) all
  // go to one buyer, or a sale where buyer and seller are the same wallet. A
  // collector buying a few cheap pieces from one artist is not wash.
  const byCreator = {};
  for (const s of sales) {
    const c = s.token?.creators?.[0]?.creator_address;
    if (!c) continue;
    (byCreator[c] ??= new Set()).add(s.buyer_address);
    byCreator[c].n = (byCreator[c].n || 0) + 1;
    byCreator[c].xtz = (byCreator[c].xtz || 0) + tez(s.price);
  }
  const wash = new Set(Object.entries(byCreator).filter(([, b]) => b.size === 1 && b.n >= 3 && b.xtz >= 500).map(([c]) => c));
  const isWash = (s) => s.buyer_address === s.seller_address || wash.has(s.token?.creators?.[0]?.creator_address);
  const clean = sales.filter((s) => !isWash(s));
  const washed = sales.filter(isWash);

  const bands = [[0, 1], [1, 5], [5, 20], [20, 100], [100, 1000], [1000, Infinity]].map(([lo, hi]) => {
    const x = clean.filter((s) => tez(s.price) >= lo && tez(s.price) < hi);
    return { xtz: hi === Infinity ? `${lo}+` : `${lo}-${hi}`, sales: x.length, volume: round(x.reduce((a, s) => a + tez(s.price), 0), 0) };
  });
  const rank = (key) => {
    const m = {};
    for (const s of clean) {
      const k = key(s);
      if (!k) continue;
      const g = (m[k] ??= { sales: 0, volume: 0, buyers: new Set() });
      g.sales++; g.volume += tez(s.price); g.buyers.add(s.buyer_address);
    }
    return Object.entries(m).sort((a, b) => b[1].volume - a[1].volume).slice(0, top)
      .map(([name, g]) => ({ name, sales: g.sales, volume: round(g.volume, 0), buyers: g.buyers.size, avg: round(g.volume / g.sales, 1) }));
  };
  const ours = new Set(Object.values(TEZOS));
  const mine = {};
  for (const s of clean) {
    if (!s.token?.creators?.some((c) => ours.has(c.creator_address))) continue;
    const g = (mine[s.token.fa?.name || s.token.fa?.contract] ??= { sales: 0, volume: 0 });
    g.sales++; g.volume = round(g.volume + tez(s.price), 2);
  }
  const volume = clean.reduce((a, s) => a + tez(s.price), 0);
  const { xtz } = await prices();
  return {
    days, sales: clean.length, volumeXtz: round(volume, 0), volumeUsd: xtz && round(volume * xtz, 0),
    buyers: new Set(clean.map((s) => s.buyer_address)).size,
    excludedAsWash: { sales: washed.length, volumeXtz: round(washed.reduce((a, s) => a + tez(s.price), 0), 0) },
    priceBands: bands,
    topCollections: rank((s) => s.token?.fa?.name || s.token?.fa?.contract),
    topCreators: rank((s) => { const c = s.token?.creators?.[0]; return c && (c.holder?.alias || c.creator_address); }),
    topSales: [...clean].sort((a, b) => b.price - a.price).slice(0, 10).map((s) => ({
      xtz: tez(s.price), token: s.token?.name, collection: s.token?.fa?.name,
      creator: s.token?.creators?.[0]?.holder?.alias || s.token?.creators?.[0]?.creator_address,
    })),
    ourSalesByCollection: mine,
  };
}

// 🗓️ The daily token

// jasellite's login shell is fish, so scripts go through `bash -s` on stdin.
function sshScript(script, timeout) {
  return new Promise((resolve, reject) => {
    const child = execFile("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=8", "jasellite", "bash", "-s"],
      { timeout, maxBuffer: 8 * 1024 * 1024 }, (err, stdout, stderr) => err ? reject(new Error((stderr || err.message).trim().slice(-600))) : resolve(stdout));
    child.stdin.end(script);
  });
}

async function dailyStatus({ limit = 14 } = {}) {
  const tokens = await get(`${TZKT}/tokens?contract=${HEN}&firstMinter=${TEZOS.aesthetic}&firstTime.ge=${DAILY_SINCE}&sort.desc=id&limit=${Number(limit) || 14}&select=tokenId,metadata.name%20as%20name,totalSupply,firstTime`);
  const rows = [];
  for (const t of tokens) {
    // objkt asks don't escrow, so every edition that left aesthetic.tez sold.
    const out = await get(`${TZKT}/tokens/transfers?token.contract=${HEN}&token.tokenId=${t.tokenId}&from=${TEZOS.aesthetic}&select=amount,to.address%20as%20to,timestamp&limit=100`);
    rows.push({
      objkt: Number(t.tokenId), name: t.name, minted: t.firstTime, editions: Number(t.totalSupply),
      sold: out.reduce((a, x) => a + Number(x.amount), 0), buyers: new Set(out.map((x) => x.to)).size,
      url: `https://objkt.com/tokens/hicetnunc/${t.tokenId}`,
    });
  }
  const balance = tez(await get(`${TZKT}/accounts/${TEZOS.aesthetic}/balance`));
  const log = await settle(sshScript("tail -40 ~/.podcast-daily.log; echo; ls -t ~/aesthetic-computer/marketing/podcast/out/daily/*.token.json 2>/dev/null | head -3 | xargs -r tail -n +1", 20_000));
  return {
    gas: runway(balance),
    tokens: rows,
    soldTotal: rows.reduce((a, r) => a + r.sold, 0),
    jasellite: log.error ? { unreachable: log.error.slice(0, 300) } : { recent: log.slice(-5000) },
  };
}

async function dailyRun({ date, confirm } = {}) {
  if (!/^\d{4}-\d\d-\d\d$/.test(date || "")) throw new Error("date must be YYYY-MM-DD");
  if (confirm !== true) {
    return { dryRun: true, note: `This mints and lists ${date}'s daily token as aesthetic.tez on jasellite (resuming from its receipt if a run stopped part-way). Call again with confirm: true.` };
  }
  const out = await sshScript(`set -e
export PATH="/usr/local/bin:/usr/bin:/bin:$PATH" TZ=America/New_York
set -a; . ~/.config/ac/claude.env; . ~/.config/ac/tezos-daily.env; set +a
cd ~/aesthetic-computer && git fetch -q origin main && git merge --ff-only -q origin/main
cd marketing/podcast && node bin/daily-token.mjs --date ${date} 2>&1`, 900_000);
  return { output: out.slice(-4000) };
}

// 💸 Transfers, prepared — never sent

async function transferPrepare({ kind = "transfer", from, to, amount } = {}) {
  const who = (address) => Object.entries(TEZOS).find(([, a]) => a === address)?.[0];
  const checks = [];
  if (kind === "withdraw_keeps_fees") {
    const dest = to || TEZOS.aesthetic;
    if (!/^tz[1-4][1-9A-HJ-NP-Za-km-z]{33}$/.test(dest)) throw new Error("to must be a tz address");
    const available = tez(await get(`${TZKT}/accounts/${KEEPS}/balance`));
    if (available === 0) checks.push("✗ the Keeps contract holds 0 XTZ; nothing to withdraw");
    if (!who(dest)) checks.push(`! ${dest} is not one of AC's wallets — check it`);
    return {
      summary: `Withdraw ${available} XTZ of Keeps fees to ${who(dest) || dest}, signed by the Keeps admin (keeps.tez).`,
      checks, command: `! cd ${ROOT}/tezos && node keeps.mjs withdraw ${dest} mainnet --wallet=keeps`,
    };
  }
  if (kind !== "transfer") throw new Error("kind must be transfer or withdraw_keeps_fees");
  if (!TEZOS[from]) throw new Error(`from must be one of ${Object.keys(TEZOS).join(", ")}`);
  if (!/^(tz[1-4]|KT1)[1-9A-HJ-NP-Za-km-z]{33}$/.test(to || "")) throw new Error("to must be a tz or KT1 address");
  amount = Number(amount);
  if (!(amount > 0)) throw new Error("amount must be a positive number of XTZ");
  const balance = tez(await get(`${TZKT}/accounts/${TEZOS[from]}/balance`));
  const fee = 0.002; // a plain transfer; a first transfer to a new tz address also burns 0.257
  const fresh = to.startsWith("tz") && (await settle(get(`${TZKT}/accounts/${to}`))).type === "empty";
  const cost = amount + fee + (fresh ? 0.257 : 0);
  if (cost > balance) checks.push(`✗ ${from} holds ${balance} XTZ; this needs ${round(cost, 6)}`);
  if (fresh) checks.push(`! ${to} has never been used on chain (0.257 XTZ allocation burn) — check the address`);
  if (!who(to)) checks.push(`! ${to} is not one of AC's wallets`);
  if (from === "aesthetic") checks.push(`daily-token gas after: ${runway(balance - cost).daysLeft} days`);
  if (to === TEZOS.aesthetic) checks.push(`daily-token gas after: ${runway(tez(await get(`${TZKT}/accounts/${to}/balance`)) + amount).daysLeft} days`);
  return {
    summary: `Send ${amount} XTZ from ${from} (${TEZOS[from]}, holds ${balance}) to ${who(to) || to}.`,
    checks, command: `! cd ${ROOT}/tezos && node transfer.mjs ${from} ${to} ${amount} mainnet`,
  };
}

// 🔌 MCP

const TOOLS = [
  {
    name: "crypto_wallets",
    description: "Every AC wallet's balance (Tezos, Ethereum, Base, Solana) from public APIs, with USD prices, the XTZ withdrawable from the Keeps contract, open offers and listings on Keeps, and how many days of gas the daily token has left. Read-only.",
    inputSchema: { type: "object", properties: {} },
  },
  {
    name: "crypto_market",
    description: "The Tezos art market on objkt over the last N days: volume, buyers, price bands, top collections, creators and sales, and sales of AC's own work by contract. Suspected wash trades are excluded and counted separately. Read-only.",
    inputSchema: {
      type: "object",
      properties: {
        days: { type: "number", description: "Window in days (default 7, max 30)" },
        top: { type: "number", description: "Rows per ranking (default 15)" },
      },
    },
  },
  {
    name: "daily_token_status",
    description: "The daily token (each night's podcast update, set in AC's pixel font and minted as a hic et nunc 1/1 by jasellite): recent OBJKTs with editions sold, days of gas left on aesthetic.tez, and jasellite's latest log and receipts when it's reachable. Read-only.",
    inputSchema: { type: "object", properties: { limit: { type: "number", description: "How many recent tokens (default 14)" } } },
  },
  {
    name: "daily_token_run",
    description: "Mint and list one date's daily token on jasellite, or resume a run that stopped part-way. Signs as aesthetic.tez. Without confirm: true it only describes what would happen.",
    inputSchema: {
      type: "object",
      properties: {
        date: { type: "string", description: "Episode date, YYYY-MM-DD" },
        confirm: { type: "boolean", description: "true to actually mint and list" },
      },
      required: ["date"],
    },
  },
  {
    name: "crypto_transfer_prepare",
    description: "Check an XTZ transfer between AC wallets (or out) or a Keeps fee withdrawal — balances, fees, whether the destination is ours, the effect on daily-token gas — and return the exact command. Never sends: @jeffrey approves by running the command.",
    inputSchema: {
      type: "object",
      properties: {
        kind: { type: "string", enum: ["transfer", "withdraw_keeps_fees"], description: "Default transfer" },
        from: { type: "string", enum: Object.keys(TEZOS), description: "Sending wallet (transfer)" },
        to: { type: "string", description: "Destination tz/KT1 address (withdraw defaults to aesthetic.tez)" },
        amount: { type: "number", description: "XTZ (transfer)" },
      },
    },
  },
];

async function callTool(name, args = {}) {
  const run = { crypto_wallets: wallets, crypto_market: market, daily_token_status: dailyStatus, daily_token_run: dailyRun, crypto_transfer_prepare: transferPrepare }[name];
  if (!run) throw new Error(`unknown tool ${name}`);
  return [{ type: "text", text: JSON.stringify(await run(args), null, 2) }];
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: params?.protocolVersion || "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "crypto-mcp", version: "1.0.0" },
          },
        };
      case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 0);
if (port) serveHttp({ handleMessage, port, banner: "🪙 crypto-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "🪙 crypto-mcp started (crypto_wallets, crypto_market, daily_token_status, daily_token_run, crypto_transfer_prepare)" });
