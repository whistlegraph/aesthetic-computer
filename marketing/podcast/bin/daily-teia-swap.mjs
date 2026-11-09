#!/usr/bin/env node
// daily-teia-swap.mjs — give every unsold daily a buy button on Teia.
//
// The nightly used to list each daily as an objkt ask, which Teia's storefront
// doesn't show. This walks the daily receipts (out/daily/*.token.json), and
// for every OBJKT aesthetic.tez still holds that has no Teia swap, sends one
// batch: retract its open objkt asks, make the Teia marketplace an operator,
// and swap the held editions on Teia's Community Marketplace. The receipt
// gets teiaSwapOp and teiaUrl. Only tokens with a daily receipt are touched;
// anything else aesthetic.tez holds is left alone.
//
// Usage:
//   node bin/daily-teia-swap.mjs --env ~/.config/ac/tezos-daily.env --dry   # the plan, nothing sent
//   node bin/daily-teia-swap.mjs --env ~/.config/ac/tezos-daily.env
//   node bin/daily-teia-swap.mjs --token 886432                              # one OBJKT
//
// Secrets as in daily-token.mjs: AESTHETIC_KEY from the environment or --env <file>.
// Tuning: DAILY_PRICE_XTZ (5), DAILY_ROYALTIES_PERMILLE (150).

import { readdirSync, readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..", "..");

const argv = process.argv.slice(2);
const flags = { env: [] };
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (!a.startsWith("--")) continue;
  const k = a.slice(2), nx = argv[i + 1];
  const v = nx !== undefined && !nx.startsWith("--") ? (i++, nx) : true;
  if (k === "env") flags.env.push(v); else flags[k] = v;
}

for (const file of flags.env) {
  for (const line of readFileSync(file, "utf8").split("\n")) {
    const m = line.match(/^\s*(?:export\s+)?([A-Z0-9_]+)\s*=\s*"?([^"\n]*)"?\s*$/);
    if (m && process.env[m[1]] === undefined) process.env[m[1]] = m[2];
  }
}

const SIGNER = "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"; // aesthetic.tez
const HEN_OBJKTS = "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
const OBJKT_MARKET = "KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X"; // objkt marketplace v6.2
const TEIA_MARKET = "KT1PHubm9HtyQEJ4BBpMTVomq6mhbfNZ9z5w"; // Teia Community Marketplace
const RPC = process.env.TEZOS_RPC || "https://rpc.tzkt.io/mainnet";
const TZKT = "https://api.tzkt.io/v1";
const PRICE_XTZ = Number(process.env.DAILY_PRICE_XTZ || 5);
const ROYALTIES = Number(process.env.DAILY_ROYALTIES_PERMILLE || 150); // Teia is per-mille
const MIN_BALANCE_XTZ = 5;

// ── the series ───────────────────────────────────────────────────────────
// A daily is a receipt with a tokenId; that file is the only membership list.
const dailyDir = resolve(ROOT, "out", "daily");
const receipts = readdirSync(dailyDir).filter((f) => f.endsWith(".token.json")).map((f) => {
  const path = resolve(dailyDir, f);
  return { path, receipt: JSON.parse(readFileSync(path, "utf8")) };
}).filter(({ receipt }) => receipt.tokenId !== undefined && !receipt.dryRun);
const byToken = new Map(receipts.map((r) => [String(r.receipt.tokenId), r]));
if (flags.token !== undefined) {
  const id = String(flags.token);
  if (!byToken.has(id)) { console.error(`✗ OBJKT #${id} has no daily receipt; refusing`); process.exit(1); }
  for (const k of [...byToken.keys()]) if (k !== id) byToken.delete(k);
}

// ── what's on chain ──────────────────────────────────────────────────────
const tzkt = async (path) => (await fetch(`${TZKT}${path}`)).json();
const bigmap = async (contract, path) => {
  const ptr = (await tzkt(`/contracts/${contract}/bigmaps?select=path,ptr`)).find((m) => m.path === path)?.ptr;
  if (ptr === undefined) throw new Error(`${contract} has no ${path} bigmap`);
  return ptr;
};
const asksMap = await bigmap(OBJKT_MARKET, "asks");
const swapsMap = await bigmap(TEIA_MARKET, "swaps");
const held = new Map((await tzkt(`/tokens/balances?account=${SIGNER}&token.contract=${HEN_OBJKTS}&balance.gt=0&limit=500&select=token.tokenId,balance`))
  .map((b) => [String(b["token.tokenId"]), Number(b.balance)]));
const asks = (await tzkt(`/bigmaps/${asksMap}/keys?active=true&value.creator=${SIGNER}&value.token.address=${HEN_OBJKTS}&select=key,value&limit=500`)) || [];
const swaps = (await tzkt(`/bigmaps/${swapsMap}/keys?active=true&value.issuer=${SIGNER}&value.fa2=${HEN_OBJKTS}&select=key,value&limit=500`)) || [];

const plan = [], skipped = [];
let sold = 0;
for (const [id, { path, receipt }] of [...byToken].sort((a, b) => Number(a[0]) - Number(b[0]))) {
  const editions = held.get(id) || 0;
  if (!editions) { sold++; continue; }
  const swap = swaps.find((s) => String(s.value.objkt_id) === id);
  if (swap) { skipped.push(`#${id} ${receipt.slug}: already on Teia (swap ${swap.key})`); continue; }
  const open = asks.filter((a) => String(a.value.token.token_id) === id);
  plan.push({ id, path, receipt, editions, askIds: open.map((a) => String(a.key)) });
}
// The rest of what aesthetic.tez holds (collected work, paintings) is none
// of this script's business; an open ask on one is only worth a mention.
const others = [...held.keys()].filter((id) => !byToken.has(id));
if (flags.token === undefined) {
  for (const a of asks) if (others.includes(String(a.value.token.token_id))) skipped.push(`#${a.value.token.token_id}: open objkt ask ${a.key} but no daily receipt; untouched`);
  skipped.push(`${others.length} other OBJKTs held, not dailies; untouched`);
}
if (sold) skipped.unshift(`${sold} dailies sold`);
if (plan.length) {
  const names = await tzkt(`/tokens?contract=${HEN_OBJKTS}&tokenId.in=${plan.map((p) => p.id).join(",")}&select=tokenId,metadata.name&limit=500`);
  for (const p of plan) p.name = names.find((n) => String(n.tokenId) === p.id)?.["metadata.name"] || p.receipt.slug;
}

for (const p of plan) {
  const asksNote = p.askIds.length ? `retract ask ${p.askIds.join(", ")}` : "no objkt ask";
  console.log(`▸ OBJKT #${p.id} "${p.name}" (${p.receipt.slug}): ${asksNote}; swap ${p.editions} × ${PRICE_XTZ} XTZ, royalties ${ROYALTIES}‰`);
}
for (const s of skipped) console.log(`  - ${s}`);
console.log(`${plan.length} to swap, ${skipped.length} skipped`);
if (!plan.length) process.exit(0);
if (flags.dry) { console.log("✓ dry run: nothing sent"); process.exit(0); }

// ── sign ─────────────────────────────────────────────────────────────────
// Taquito lives in tezos/'s node_modules (npm install --prefix tezos).
const requireTezos = createRequire(resolve(REPO, "tezos", "package.json"));
const { TezosToolkit } = requireTezos("@taquito/taquito");
const { InMemorySigner } = requireTezos("@taquito/signer");

if (!process.env.AESTHETIC_KEY) { console.error("✗ AESTHETIC_KEY not set"); process.exit(1); }
const tezos = new TezosToolkit(RPC);
tezos.setProvider({ signer: await InMemorySigner.fromSecretKey(process.env.AESTHETIC_KEY) });
const signer = await tezos.signer.publicKeyHash();
if (signer !== SIGNER) { console.error(`✗ key belongs to ${signer}, not aesthetic.tez; refusing`); process.exit(1); }

const token = await tezos.contract.at(HEN_OBJKTS);
const objkt = await tezos.contract.at(OBJKT_MARKET);
const teia = await tezos.contract.at(TEIA_MARKET);

// One batch per token, asks retracted first, so no edition is ever on both
// markets and a failure leaves the rest of the series exactly as it was.
let failures = 0;
for (const p of plan) {
  const balance = (await tezos.tz.getBalance(signer)).toNumber() / 1e6;
  if (balance < MIN_BALANCE_XTZ) { console.error(`✗ aesthetic.tez holds ${balance} XTZ (< ${MIN_BALANCE_XTZ}); stopping`); process.exit(1); }
  let batch = tezos.contract.batch();
  for (const askId of p.askIds) batch = batch.withContractCall(objkt.methodsObject.retract_ask(askId));
  batch = batch
    .withContractCall(token.methodsObject.update_operators([
      { add_operator: { owner: SIGNER, operator: TEIA_MARKET, token_id: p.id } },
    ]))
    .withContractCall(teia.methodsObject.swap({
      fa2: HEN_OBJKTS,
      objkt_id: p.id,
      objkt_amount: String(p.editions),
      xtz_per_objkt: String(Math.round(PRICE_XTZ * 1e6)),
      royalties: String(ROYALTIES),
      creator: SIGNER,
    }));
  try {
    const op = await batch.send();
    console.log(`  #${p.id} swapping… ${op.hash}`);
    await op.confirmation(1);
    Object.assign(p.receipt, {
      teiaSwapOp: op.hash, listedOn: "teia", priceXtz: PRICE_XTZ, editions: p.editions,
      teiaUrl: `https://teia.art/objkt/${p.id}`,
    });
    writeFileSync(p.path, JSON.stringify(p.receipt, null, 2) + "\n");
    console.log(`✓ OBJKT #${p.id} "${p.name}" — ${p.editions} × ${PRICE_XTZ} XTZ — ${p.receipt.teiaUrl} — ${op.hash}`);
  } catch (err) {
    failures++;
    console.error(`✗ OBJKT #${p.id}: ${err.message?.split("\n")[0] || err}`);
    if (failures >= 2) { console.error("✗ two failures; stopping rather than retrying blindly"); process.exit(1); }
  }
}
const after = (await tezos.tz.getBalance(signer)).toNumber() / 1e6;
console.log(`aesthetic.tez holds ${after.toFixed(3)} XTZ`);
process.exit(failures ? 1 : 0);
