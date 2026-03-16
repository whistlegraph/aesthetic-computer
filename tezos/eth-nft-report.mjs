#!/usr/bin/env node
// ETH NFT Holdings Report with floor prices
// Usage: node eth-nft-report.mjs

const KEY = process.env.ETHERSCAN_API_KEY;
if (!KEY) { console.error('❌ Set ETHERSCAN_API_KEY env var'); process.exit(1); }

const WALLETS = [
  { name: '4esthetic.eth', address: '0x5e6758C96A4cB5E2A1FE2E2772020dc8ad753b08' },
  { name: 'whistlegraph.eth', address: '0x238c9c645c6EE83d4323A2449C706940321a0cBf' },
  { name: 'Unknown ETH', address: '0x98eAc86755792e03D0f027cA8CcFa83818B994c4' },
];

async function getHoldings(address) {
  // Get ERC-721 transfers
  const url = `https://api.etherscan.io/v2/api?chainid=1&module=account&action=tokennfttx&address=${address}&startblock=0&endblock=99999999&sort=asc&apikey=${KEY}`;
  const resp = await fetch(url);
  const data = await resp.json();
  const txs = data.result || [];

  const addrLower = address.toLowerCase();
  const held = {};
  for (const t of txs) {
    const k = t.contractAddress + '/' + t.tokenID;
    if (t.to.toLowerCase() === addrLower) {
      held[k] = { name: t.tokenName || t.tokenSymbol, id: t.tokenID, contract: t.contractAddress };
    } else {
      delete held[k];
    }
  }

  // Also get ERC-1155
  const url1155 = `https://api.etherscan.io/v2/api?chainid=1&module=account&action=token1155tx&address=${address}&startblock=0&endblock=99999999&sort=asc&apikey=${KEY}`;
  const resp1155 = await fetch(url1155);
  const data1155 = await resp1155.json();
  const txs1155 = data1155.result || [];

  for (const t of txs1155) {
    const k = t.contractAddress + '/1155/' + t.tokenID;
    const val = parseInt(t.tokenValue || '1');
    if (t.to.toLowerCase() === addrLower) {
      if (!held[k]) held[k] = { name: t.tokenName || t.tokenSymbol, id: t.tokenID, contract: t.contractAddress, is1155: true, balance: 0 };
      held[k].balance = (held[k].balance || 0) + val;
    } else if (held[k]) {
      held[k].balance = (held[k].balance || 0) - val;
      if (held[k].balance <= 0) delete held[k];
    }
  }

  // Group by collection
  const collections = {};
  for (const t of Object.values(held)) {
    if (!collections[t.contract]) collections[t.contract] = { name: t.name, tokens: [], contract: t.contract, is1155: t.is1155 };
    collections[t.contract].tokens.push(t.id);
  }

  return Object.values(collections).sort((a, b) => b.tokens.length - a.tokens.length);
}

async function getFloorPrice(contractAddress) {
  // Use Etherscan token info + OpenSea collection stats via slug
  // Try Blur API (no key needed for basic stats)
  try {
    const resp = await fetch(`https://api.blur.io/v1/collections/${contractAddress}`, {
      headers: { 'User-Agent': 'Mozilla/5.0' }
    });
    if (resp.ok) {
      const d = await resp.json();
      if (d.collection?.floorPrice) return { floor: parseFloat(d.collection.floorPrice), source: 'Blur' };
    }
  } catch {}

  // Try reservoir
  try {
    const resp = await fetch(`https://api.reservoir.tools/collections/v7?contract=${contractAddress}&limit=1`);
    if (resp.ok) {
      const d = await resp.json();
      const c = d.collections?.[0];
      if (c?.floorAsk?.price?.amount?.decimal) {
        return { floor: c.floorAsk.price.amount.decimal, source: 'Reservoir', topBid: c.topBid?.price?.amount?.decimal };
      }
    }
  } catch {}

  return null;
}

console.log('\n╔══════════════════════════════════════════════════════════════╗');
console.log('║  🖼️  ETH NFT Holdings Report                                ║');
console.log('╚══════════════════════════════════════════════════════════════╝\n');

let grandTotal = 0;
let totalEstValue = 0;

for (const wallet of WALLETS) {
  console.log(`\n🔑 ${wallet.name} (${wallet.address})`);
  console.log('═'.repeat(62));

  const collections = await getHoldings(wallet.address);
  const totalTokens = collections.reduce((s, c) => s + c.tokens.length, 0);
  console.log(`📦 ${totalTokens} tokens across ${collections.length} collections\n`);
  grandTotal += totalTokens;

  if (!collections.length) {
    console.log('  (empty)\n');
    continue;
  }

  let walletValue = 0;

  for (const col of collections) {
    const count = col.tokens.length;
    const type = col.is1155 ? 'ERC-1155' : 'ERC-721';

    // Rate limit: small delay between floor price lookups
    await new Promise(r => setTimeout(r, 200));
    const price = await getFloorPrice(col.contract);

    const floorStr = price ? `${price.floor.toFixed(4)} ETH` : '—';
    const bidStr = price?.topBid ? ` | top bid: ${price.topBid.toFixed(4)} ETH` : '';
    const estValue = price ? price.floor * count : 0;
    walletValue += estValue;

    console.log(`  📛 ${col.name} (${count}) [${type}]`);
    console.log(`     📍 ${col.contract}`);
    console.log(`     💰 Floor: ${floorStr}${bidStr}`);
    if (estValue > 0) console.log(`     📊 Est. value: ${estValue.toFixed(4)} ETH (${count} × ${price.floor.toFixed(4)})`);
    if (count <= 8) console.log(`     🔢 IDs: ${col.tokens.join(', ')}`);
    else console.log(`     🔢 IDs: ${col.tokens.slice(0, 5).join(', ')} ... (+${count - 5} more)`);
    console.log('');
  }

  totalEstValue += walletValue;
  console.log(`  💎 Wallet est. total: ${walletValue.toFixed(4)} ETH`);
  console.log('');
}

// Get ETH price
let ethUsd = 0;
try {
  const resp = await fetch('https://api.coingecko.com/api/v3/simple/price?ids=ethereum&vs_currencies=usd');
  if (resp.ok) {
    const d = await resp.json();
    ethUsd = d.ethereum?.usd || 0;
  }
} catch {}

console.log('\n' + '═'.repeat(62));
console.log(`📊 GRAND TOTAL: ${grandTotal} NFTs`);
console.log(`💎 Est. floor value: ${totalEstValue.toFixed(4)} ETH`);
if (ethUsd > 0) console.log(`💵 Est. USD value: $${(totalEstValue * ethUsd).toFixed(2)} (ETH @ $${ethUsd.toFixed(0)})`);
console.log('');
