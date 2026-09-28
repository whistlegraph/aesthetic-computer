#!/usr/bin/env node
// daily-reprice.mjs — move a minted daily token to a new price.
//
// An objkt ask can't be edited, so this retracts aesthetic.tez's open asks on
// the day's OBJKT and lists what aesthetic.tez still holds at the new price,
// in one batch. The day's receipt (out/daily/<slug>.token.json) is updated.
//
// Usage:
//   node bin/daily-reprice.mjs --date 2026-09-27 --price 3
//   node bin/daily-reprice.mjs --date 2026-09-27 --price 3 --dry
//
// Secrets as in daily-token.mjs: AESTHETIC_KEY from the environment or --env <file>.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
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

const date = flags.date;
const price = Number(flags.price);
if (typeof date !== "string" || !/^\d{4}-\d{2}-\d{2}$/.test(date)) { console.error("✗ --date YYYY-MM-DD required"); process.exit(1); }
if (!(price > 0)) { console.error("✗ --price <xtz> required"); process.exit(1); }
const slug = `daily-${date}`;

const SIGNER = "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"; // aesthetic.tez
const HEN_OBJKTS = "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
const OBJKT_MARKET = "KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X"; // objkt marketplace v6.2
const RPC = process.env.TEZOS_RPC || "https://rpc.tzkt.io/mainnet";
const TZKT = "https://api.tzkt.io/v1";
const ROYALTIES = Number(process.env.DAILY_ROYALTIES_PERMILLE || 150);

const receiptPath = resolve(ROOT, "out", "daily", `${slug}.token.json`);
if (!existsSync(receiptPath)) { console.error(`✗ no receipt at ${receiptPath}`); process.exit(1); }
const receipt = JSON.parse(readFileSync(receiptPath, "utf8"));
if (receipt.tokenId === undefined) { console.error(`✗ ${slug} was never minted`); process.exit(1); }
const tokenId = String(receipt.tokenId);

const tzkt = async (path) => (await fetch(`${TZKT}${path}`)).json();
// TzKT ignores a path filter here, so pick the asks map out of the list.
const asksMap = (await tzkt(`/contracts/${OBJKT_MARKET}/bigmaps?select=path,ptr`)).find((m) => m.path === "asks")?.ptr;
if (asksMap === undefined) { console.error("✗ objkt market has no asks bigmap"); process.exit(1); }
const asks = (await tzkt(`/bigmaps/${asksMap}/keys?active=true&value.creator=${SIGNER}` +
  `&value.token.address=${HEN_OBJKTS}&value.token.token_id=${tokenId}&select=key,value`)) || [];
const [held] = await tzkt(`/tokens/balances?account=${SIGNER}&token.contract=${HEN_OBJKTS}&token.tokenId=${tokenId}&select=balance`);
const editions = Number(held || 0);

for (const a of asks) console.log(`  ask #${a.key}: ${a.value.editions} × ${Number(a.value.amount) / 1e6} XTZ`);
console.log(`▸ OBJKT #${tokenId}: aesthetic.tez holds ${editions}; relisting at ${price} XTZ`);
if (!editions) { console.error("✗ nothing left to list"); process.exit(1); }
if (flags.dry) process.exit(0);

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
const market = await tezos.contract.at(OBJKT_MARKET);
let batch = tezos.contract.batch();
for (const a of asks) batch = batch.withContractCall(market.methodsObject.retract_ask(a.key));
// The operator from the first listing is kept; adding it again is harmless.
batch = batch
  .withContractCall(token.methodsObject.update_operators([
    { add_operator: { owner: SIGNER, operator: OBJKT_MARKET, token_id: tokenId } },
  ]))
  .withContractCall(market.methodsObject.ask({
    token: { address: HEN_OBJKTS, token_id: tokenId },
    currency: { tez: {} },
    amount: String(Math.round(price * 1e6)),
    editions: String(editions),
    shares: { [SIGNER]: String(ROYALTIES * 10) }, // objkt shares are per ten-thousand
    start_time: null,
    expiry_time: null,
    referral_bonus: "500",
    condition: null,
  }));
const op = await batch.send();
await op.confirmation(1);

Object.assign(receipt, { listOp: op.hash, priceXtz: price, repricedAt: new Date().toISOString() });
writeFileSync(receiptPath, JSON.stringify(receipt, null, 2) + "\n");
console.log(`✓ ${slug}: OBJKT #${tokenId} — ${editions} × ${price} XTZ — ${op.hash}`);
