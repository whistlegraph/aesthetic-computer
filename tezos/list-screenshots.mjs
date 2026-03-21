#!/usr/bin/env node
// Batch list Screenshots NFTs on OpenSea at $15 each
// Usage: node list-screenshots.mjs [--dry-run]

import { OpenSeaSDK, Chain } from 'opensea-js';
import { ethers } from 'ethers';

const SCREENSHOTS_CONTRACT = '0x651c6dc799864004a9df503db6a570c4c1cec743';
const PRICE_USD = 15;
const WALLET_MNEMONIC = process.env.ETH_MNEMONIC;

const TOKEN_IDS = [
  16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,
  36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,
  56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,
  76,77,78,79,80,81,83,84,85,86,87,88,89,90
];

const dryRun = process.argv.includes('--dry-run');

async function main() {
  if (!WALLET_MNEMONIC) {
    console.error('❌ Set ETH_MNEMONIC env var (citizen wallet mnemonic)');
    process.exit(1);
  }

  // Get current ETH price
  let ethUsd = 2174; // fallback
  try {
    const resp = await fetch('https://api.coingecko.com/api/v3/simple/price?ids=ethereum&vs_currencies=usd');
    if (resp.ok) {
      const d = await resp.json();
      ethUsd = d.ethereum?.usd || ethUsd;
    }
  } catch {}

  const priceEth = PRICE_USD / ethUsd;

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🖼️  Batch List Screenshots on OpenSea                       ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  console.log(`📍 Contract:    ${SCREENSHOTS_CONTRACT}`);
  console.log(`🎨 Tokens:      ${TOKEN_IDS.length}`);
  console.log(`💵 Price:       $${PRICE_USD} each (${priceEth.toFixed(6)} ETH @ $${ethUsd})`);
  console.log(`💰 Total if sold: $${PRICE_USD * TOKEN_IDS.length}`);
  console.log(`🔧 Mode:        ${dryRun ? 'DRY RUN' : 'LIVE'}\n`);

  // Set up wallet
  const provider = new ethers.JsonRpcProvider('https://rpc.mevblocker.io');
  const wallet = ethers.Wallet.fromPhrase(WALLET_MNEMONIC).connect(provider);
  console.log(`👤 Wallet:      ${wallet.address}\n`);

  // Set up OpenSea SDK
  const sdk = new OpenSeaSDK(wallet, {
    chain: Chain.Mainnet,
    apiKey: process.env.OPENSEA_API_KEY,
  });

  if (dryRun) {
    console.log('⚠️  DRY RUN — no listings will be created.\n');
    console.log('Would list:');
    for (const id of TOKEN_IDS) {
      console.log(`  Screenshots #${id} @ ${priceEth.toFixed(6)} ETH ($${PRICE_USD})`);
    }
    console.log(`\nTotal: ${TOKEN_IDS.length} listings`);
    return;
  }

  // Build bulk listings array — start with just first 3 as test
  const testMode = process.argv.includes('--test');
  const idsToList = testMode ? TOKEN_IDS.slice(0, 3) : TOKEN_IDS;
  const listings = idsToList.map(tokenId => ({
    asset: {
      tokenId: tokenId.toString(),
      tokenAddress: SCREENSHOTS_CONTRACT,
    },
    amount: priceEth,
    expirationTime: Math.round(Date.now() / 1000) + 90 * 24 * 60 * 60, // 90 days
  }));

  console.log(`📝 Submitting ${listings.length} bulk listings...\n`);

  const result = await sdk.createBulkListings({
    listings,
    accountAddress: wallet.address,
    continueOnError: true,
    onProgress: (completed, total) => {
      process.stdout.write(`\r  Progress: ${completed}/${total}`);
    },
  });

  const success = result.successful?.length || 0;
  const failed = result.failed?.length || 0;
  const errors = result.failed || [];

  console.log('\n');
  for (const s of (result.successes || [])) {
    // just count, don't spam
  }

  console.log(`\n${'═'.repeat(50)}`);
  console.log(`✅ Listed: ${success}`);
  console.log(`❌ Failed: ${failed}`);
  if (errors.length) {
    console.log('\nErrors:');
    for (const e of errors) {
      console.log(`  ${JSON.stringify(e).slice(0, 100)}`);
    }
  }
}

main().catch(e => { console.error('❌', e.message); process.exit(1); });
