#!/usr/bin/env node
// ETH NFT Holdings Report with floor prices
// Usage: node eth-nft-report.mjs

const KEY = process.env.ETHERSCAN_API_KEY;
if (!KEY) { console.error('❌ Set ETHERSCAN_API_KEY env var'); process.exit(1); }

// Floor-price providers (any one enables valuations). Priority: Alchemy → OpenSea.
// (Reservoir was dropped — Reservoir Tools wound down its NFT API and pivoted to relay.link.)
const ALCHEMY_KEY = process.env.ALCHEMY_API_KEY;
const OPENSEA_KEY = process.env.OPENSEA_API_KEY;
const FLOOR_ENABLED = ALCHEMY_KEY || OPENSEA_KEY;

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

const slugCache = new Map();

async function getFloorPrice(contractAddress) {
  // Alchemy: getFloorPrice takes a contract address directly, returns ETH floor.
  if (ALCHEMY_KEY) {
    try {
      const resp = await fetch(`https://eth-mainnet.g.alchemy.com/nft/v3/${ALCHEMY_KEY}/getFloorPrice?contractAddress=${contractAddress}`);
      if (resp.ok) {
        const d = await resp.json();
        const candidates = [d.openSea?.floorPrice, d.looksRare?.floorPrice].filter((v) => typeof v === 'number' && v > 0);
        if (candidates.length) return { floor: Math.min(...candidates), source: 'Alchemy' };
      }
    } catch {}
  }

  // OpenSea v2: resolve collection slug from contract, then pull stats.
  if (OPENSEA_KEY) {
    try {
      let slug = slugCache.get(contractAddress);
      if (slug === undefined) {
        const cr = await fetch(`https://api.opensea.io/api/v2/chain/ethereum/contract/${contractAddress}`, { headers: { 'x-api-key': OPENSEA_KEY } });
        slug = cr.ok ? (await cr.json()).collection || null : null;
        slugCache.set(contractAddress, slug);
      }
      if (slug) {
        const sr = await fetch(`https://api.opensea.io/api/v2/collections/${slug}/stats`, { headers: { 'x-api-key': OPENSEA_KEY } });
        if (sr.ok) {
          const s = await sr.json();
          const floor = s.total?.floor_price;
          if (typeof floor === 'number' && floor > 0) return { floor, source: 'OpenSea' };
        }
      }
    } catch {}
  }

  return null;
}

console.log('\n╔══════════════════════════════════════════════════════════════╗');
console.log('║  🖼️  ETH NFT Holdings Report                                ║');
console.log('╚══════════════════════════════════════════════════════════════╝\n');

if (!FLOOR_ENABLED) {
  console.log('ℹ️  No floor-price key set — counts only.');
  console.log('   Set ALCHEMY_API_KEY (recommended) or OPENSEA_API_KEY to value holdings.\n');
}

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
