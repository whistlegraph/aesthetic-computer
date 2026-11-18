#!/usr/bin/env node
// List the Ethereum tokens we made, on OpenSea, from 4esthetic.eth.
//
//   node list-our-eth.mjs            dry run: prints every listing, signs nothing
//   node list-our-eth.mjs --live     signs and posts the listings (gasless)
//   node list-our-eth.mjs --live --only screenshots|whistlegraph|tapes
//
// Holdings are read live from the chain, so a token that has left the wallet
// is never listed. Keys come from the vault (AC_VAULT, or the sibling
// aesthetic-computer-vault): OPENSEA_API_KEY and ALCHEMY_API_KEY from .env,
// and — in --live only — the eth-citizen mnemonic from wallets/wallets.json.
// Nothing secret is printed.
//
// --live needs the SDK:  npm install @opensea/sdk   (opensea-js was renamed)

import { readFileSync, existsSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const VAULT = process.env.AC_VAULT || resolve(HERE, "../aesthetic-computer-vault");
const OWNER = "0x5e6758C96A4cB5E2A1FE2E2772020dc8ad753b08"; // 4esthetic.eth
const DAYS = 90;

// What we made and what we ask. `price` is ETH per token; `prices` overrides by token id.
const COLLECTIONS = [
  { key: "screenshots", name: "Screenshots",
    address: "0x651c6dC799864004A9Df503db6A570c4c1CeC743", price: 0.01 },
  { key: "whistlegraph", name: "The Longest Whistlegraph Ever (so far)",
    address: "0x449DC394c46EB8c15e6af7fefb0B14B95E9c1894", price: 0.5,
    prices: { 3: 1 } }, // 4/5, the Herbert W. Franke Tribute
  { key: "tapes", name: "tapes",
    address: "0x56294e79fECA8f218D327dcD5818ADf66C4b414e", price: 0.02 },
];

const live = process.argv.includes("--live");
const only = process.argv.includes("--only")
  ? process.argv[process.argv.indexOf("--only") + 1] : null;

// Read KEY=value lines from the vault .env without touching process.env beyond what we need.
function vaultEnv(name) {
  if (process.env[name]) return process.env[name];
  const file = join(VAULT, ".env");
  if (!existsSync(file)) return undefined;
  const line = readFileSync(file, "utf8").split("\n").find((l) => l.startsWith(name + "="));
  return line?.slice(name.length + 1).trim().replace(/^["']|["']$/g, "");
}

const ALCHEMY = vaultEnv("ALCHEMY_API_KEY");
const OPENSEA = vaultEnv("OPENSEA_API_KEY");
if (!ALCHEMY) { console.error("No ALCHEMY_API_KEY in env or vault .env"); process.exit(1); }

// Every token of ours the wallet holds right now.
async function holdings(col) {
  const out = []; let pageKey;
  do {
    const url = `https://eth-mainnet.g.alchemy.com/nft/v3/${ALCHEMY}/getNFTsForOwner` +
      `?owner=${OWNER}&contractAddresses[]=${col.address}&withMetadata=true&pageSize=100` +
      (pageKey ? `&pageKey=${pageKey}` : "");
    const res = await fetch(url);
    if (!res.ok) throw new Error(`holdings ${col.name}: HTTP ${res.status}`);
    const data = await res.json();
    for (const n of data.ownedNfts || []) {
      out.push({ id: n.tokenId, name: n.name || `#${n.tokenId}`, balance: Number(n.balance || 1), type: n.tokenType });
    }
    pageKey = data.pageKey;
  } while (pageKey);
  return out.sort((a, b) => Number(a.id) - Number(b.id));
}

const plan = [];
for (const col of COLLECTIONS) {
  if (only && only !== col.key) continue;
  for (const t of await holdings(col)) {
    plan.push({ col, ...t, eth: col.prices?.[t.id] ?? col.price });
  }
}

console.log(`\n${live ? "LIVE" : "DRY RUN"} — ${plan.length} listings from ${OWNER}, ${DAYS} days\n`);
for (const col of COLLECTIONS) {
  const rows = plan.filter((p) => p.col === col);
  if (!rows.length) continue;
  const total = rows.reduce((s, r) => s + r.eth, 0);
  console.log(`${col.name} — ${rows.length} tokens, ${total.toFixed(3)} ETH if all sell`);
  for (const r of rows) {
    const extra = r.balance > 1 ? `  (holding ${r.balance}, listing 1)` : "";
    console.log(`  #${String(r.id).padEnd(4)} ${String(r.eth).padEnd(5)} ETH  ${r.name}${extra}`);
  }
  console.log();
}
const grand = plan.reduce((s, r) => s + r.eth, 0);
console.log(`Total asked: ${grand.toFixed(3)} ETH. OpenSea takes 1% of each sale.\n`);

if (!live) { console.log("Nothing signed. Re-run with --live to post these.\n"); process.exit(0); }

// ---- live: sign and post ----
if (!OPENSEA) { console.error("No OPENSEA_API_KEY in env or vault .env"); process.exit(1); }
let OpenSeaSDK, Chain;
try { ({ OpenSeaSDK, Chain } = await import("@opensea/sdk")); }
catch { console.error("Missing SDK. Run: npm install @opensea/sdk"); process.exit(1); }
const { ethers } = await import("ethers");

const wallets = JSON.parse(readFileSync(join(VAULT, "wallets/wallets.json"), "utf8")).wallets;
const provider = new ethers.JsonRpcProvider(`https://eth-mainnet.g.alchemy.com/v2/${ALCHEMY}`);
const wallet = ethers.Wallet.fromPhrase(wallets["eth-citizen"].mnemonic.trim()).connect(provider);
if (wallet.address.toLowerCase() !== OWNER.toLowerCase()) {
  console.error(`Vault key derives ${wallet.address}, expected ${OWNER}. Stopping.`); process.exit(1);
}

const sdk = new OpenSeaSDK(wallet, { chain: Chain.Mainnet, apiKey: OPENSEA });
const expirationTime = Math.round(Date.now() / 1000) + DAYS * 24 * 60 * 60;
const listings = plan.map((p) => ({
  asset: { tokenId: String(p.id), tokenAddress: p.col.address },
  amount: p.eth,
  quantity: 1,
  expirationTime,
}));

const result = await sdk.createBulkListings({
  listings,
  accountAddress: wallet.address,
  continueOnError: true,
  onProgress: (done, total) => process.stdout.write(`\r  posted ${done}/${total}`),
});

const ok = result.successful?.length || 0;
const failed = result.failed || [];
console.log(`\n\nListed: ${ok}   Failed: ${failed.length}`);
for (const f of failed) console.log("  " + JSON.stringify(f).slice(0, 160));
